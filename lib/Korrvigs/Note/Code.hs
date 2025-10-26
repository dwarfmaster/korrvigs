module Korrvigs.Note.Code where

import Control.Applicative ((<|>))
import Control.Arrow ((***))
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Compute.Runnable
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Pandoc
import Korrvigs.Utils
import Text.Parsec hiding ((<|>))
import Text.Parsec.Number

fromId :: (Applicative f) => Text -> ((Attr, Text) -> f (Attr, Text)) -> Document -> f Document
fromId nm = docContent . each . bkSubBlocks . _CodeBlock . filtered ((== nm) . view (_1 . attrId))

knownLanguages :: Map Text Executable
knownLanguages =
  M.fromList
    [ ("bash", Bash),
      ("prolog", SwiProlog)
    ]

data AttrData = AttrData
  { _attrArg :: Map Int RunArg,
    _attrEnv :: Map Text RunArg,
    _attrStdIn :: Maybe RunArg
  }
  deriving (Show)

makeLenses ''AttrData

instance Semigroup AttrData where
  dt1 <> dt2 =
    AttrData
      { _attrArg = M.union (dt1 ^. attrArg) (dt2 ^. attrArg),
        _attrEnv = M.union (dt1 ^. attrEnv) (dt2 ^. attrEnv),
        _attrStdIn = (dt1 ^. attrStdIn) <|> (dt2 ^. attrStdIn)
      }

instance Monoid AttrData where
  mempty = AttrData M.empty M.empty Nothing

parseAttrMtdt :: Text -> Text -> AttrData
parseAttrMtdt key val = case parse (argP <|> envP <|> stdinP) "<codearg>" key of
  Left _ -> mempty
  Right attrDat -> attrDat
  where
    valP :: (Stream s Identity Char) => Parsec s () RunArg
    valP = option (ArgPlain val) $ do
      void $ char ':'
      kd <- many alphaNum
      case kd of
        "comp" ->
          pure $
            uncurry ArgResult $
              MkId *** (maybe "" snd . T.uncons) $
                T.break (== '#') val
        "entry" -> pure $ ArgEntry $ MkId val
        _ -> fail $ kd <> " is not a recognised RunArg type"
    argP :: (Stream s Identity Char) => Parsec s () AttrData
    argP = do
      void $ string "arg:"
      num <- decimal
      ra <- valP
      pure $ mempty & attrArg . at num ?~ ra
    envP :: (Stream s Identity Char) => Parsec s () AttrData
    envP = do
      void $ string "env:"
      nm <- T.pack <$> many (alphaNum <|> char '_')
      ra <- valP
      pure $ mempty & attrEnv . at nm ?~ ra
    stdinP :: (Stream s Identity Char) => Parsec s () AttrData
    stdinP = do
      void $ string "stdin"
      ra <- valP
      pure $ mempty & attrStdIn ?~ ra

toRunnable :: Attr -> Text -> Maybe Runnable
toRunnable attr code = do
  let classes = attr ^. attrClasses
  let attrDat = foldMap (uncurry parseAttrMtdt) $ M.toList $ attr ^. attrMtdt
  language <- foldr ((<|>) . flip M.lookup knownLanguages) Nothing classes
  pure $
    Runnable
      { _runExecutable = language,
        _runCode = code,
        _runArgs = fmap snd $ M.toList $ attrDat ^. attrEnv,
        _runEnv = attrDat ^. attrEnv,
        _runStdIn = attrDat ^. attrStdIn
      }

codeRunnable :: (MonadKorrvigs m) => Id -> Text -> m (Maybe Runnable)
codeRunnable i codeId = runMaybeT $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? _Note
  doc <- hoistEitherLift $ readNote $ note ^. notePath
  hoistMaybe $ doc ^? fromId codeId . to (uncurry toRunnable) . _Just
