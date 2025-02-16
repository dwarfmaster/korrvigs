{-# LANGUAGE FunctionalDependencies #-}

module Korrvigs.Utils.XML
  ( TreeStream,
    TreeParseError (..),
    errStream,
    errMsg,
    TreeParserT,
    get,
    peek,
    onChildren,
    eofT,
    tillEof,
    XMLParserT,
    XMLParser,
    element,
    onElementChildren,
    content,
    runTreeParser,
  )
where

import Control.Applicative
import Control.Arrow (first)
import Control.Lens hiding (children, element)
import Control.Monad
import Control.Monad.Error.Class
import Data.Functor
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor (Cursor, child, followingSibling, node)

-- A stream with a tree structure, where each element might have a stream of children in
-- addition to the top-level stream.
class (Monad m) => TreeStream stream m val | stream -> val where
  advance :: stream -> m (Maybe stream)
  children :: stream -> m (Maybe stream)
  extract :: stream -> m val

-- Tree parser
data TreeParseError s = TreeParseError
  { _errStream :: Maybe s,
    _errMsg :: Text
  }
  deriving (Show)

makeLenses ''TreeParseError

newtype TreeParserT s m a = TreeParser (Maybe s -> m (Either (TreeParseError s) (a, Maybe s)))

instance (Functor m) => Functor (TreeParserT s m) where
  fmap f (TreeParser prs) = TreeParser $ fmap (fmap (first f)) . prs

instance (Monad m) => Applicative (TreeParserT s m) where
  pure x = TreeParser $ \s -> pure $ Right (x, s)
  liftA2 f (TreeParser prs1) (TreeParser prs2) =
    TreeParser $
      prs1 >=> \case
        Left err -> pure $ Left err
        Right (a, s) ->
          prs2 s >>= \case
            Left err -> pure $ Left err
            Right (b, s') -> pure $ Right (f a b, s')

instance (Monad m) => Monad (TreeParserT s m) where
  (TreeParser prs) >>= f =
    TreeParser $
      prs >=> \case
        Left err -> pure $ Left err
        Right (a, s) -> let TreeParser prs' = f a in prs' s

instance (Monad m) => MonadError (TreeParseError s) (TreeParserT s m) where
  throwError e = TreeParser $ const $ pure $ Left e
  catchError (TreeParser prs) f = TreeParser $ \s ->
    prs s >>= \case
      Left e -> let TreeParser prs' = f e in prs' s
      Right (x, s') -> pure $ Right (x, s')

instance (Monad m) => MonadFail (TreeParserT s m) where
  fail msg = TreeParser $ \s -> pure $ Left $ TreeParseError s $ T.pack msg

instance (Monad m) => Alternative (TreeParserT s m) where
  empty = TreeParser $ \s -> pure $ Left $ TreeParseError s "Empty parser"
  (TreeParser prs1) <|> (TreeParser prs2) = TreeParser $ \s ->
    prs1 s >>= \case
      Left _ -> prs2 s
      Right (x, s') -> pure $ Right (x, s')

instance (Monad m) => MonadPlus (TreeParserT s m) where
  mzero = empty
  mplus = (<|>)

runTreeParser :: (TreeStream stream m val) => TreeParserT stream m a -> stream -> m (Either (TreeParseError stream) a)
runTreeParser (TreeParser prs) =
  prs . Just >=> \case
    Left err -> pure $ Left err
    Right (x, _) -> pure $ Right x

peek :: (TreeStream stream m val) => TreeParserT stream m (Maybe val)
peek = TreeParser $ \s -> case s of
  Just vs -> Right . (,s) . Just <$> extract vs
  Nothing -> pure $ Right (Nothing, s)

get :: (TreeStream stream m val) => TreeParserT stream m (Maybe val)
get = TreeParser $ \s -> case s of
  Nothing -> pure $ Right (Nothing, s)
  Just vs -> do
    v <- extract vs
    ns <- advance vs
    pure $ Right (Just v, ns)

onChildren :: (TreeStream s m a) => (a -> TreeParserT s m v) -> TreeParserT s m v
onChildren f = TreeParser $ \case
  Nothing -> pure $ Left $ TreeParseError Nothing "End of stream reached"
  Just vs -> do
    tok <- extract vs
    ns <- advance vs
    cs <- children vs
    let TreeParser prs = f tok
     in prs cs >>= \case
          Left err -> pure $ Left err
          Right (v, _) -> pure $ Right (v, ns)

eofT :: (TreeStream s m a) => TreeParserT s m ()
eofT = TreeParser $ \case
  Nothing -> pure $ Right ((), Nothing)
  Just s -> pure $ Left $ TreeParseError (Just s) "Expected eof"

tillEof :: (TreeStream s m a) => TreeParserT s m v -> TreeParserT s m [v]
tillEof prs = (eofT $> []) <|> ((:) <$> prs <*> tillEof prs)

-- XML definition
instance (Monad m) => TreeStream Cursor m Node where
  advance cursor = pure $ listToMaybe $ followingSibling cursor
  children cursor = pure $ listToMaybe $ child cursor
  extract cursor = pure $ node cursor

type XMLParserT m = TreeParserT Cursor m

type XMLParser = XMLParserT Identity

element :: (Monad m) => XMLParserT m Element
element =
  get >>= \case
    Just (NodeElement el) -> pure el
    Nothing -> fail "Expected element, got end-of-stream"
    _ -> fail "Expected element"

onElementChildren :: (Monad m) => (Element -> XMLParserT m a) -> XMLParserT m a
onElementChildren f = onChildren $ asElement >=> f
  where
    asElement :: (MonadFail m) => Node -> m Element
    asElement (NodeElement el) = pure el
    asElement _ = fail "Expected element"

content :: (Monad m) => XMLParserT m Text
content =
  get >>= \case
    Just (NodeContent txt) -> pure txt
    Nothing -> fail "Expected content, got end-of-stream"
    _ -> fail "Expected content"
