module Korrvigs.Note.Pandoc (parsePandoc) where

import Control.Arrow ((&&&))
import Control.Lens hiding ((<|))
import Control.Monad.Extra (concatMapM)
import Control.Monad.Loops (whileJust_, whileM_)
import Control.Monad.State.Lazy
import Data.Aeson hiding ((.=))
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry (Metadata)
import Korrvigs.Entry.Ident
import qualified Korrvigs.Note.AST as A
import Korrvigs.Note.Helpers
import Network.URI
import Text.Pandoc hiding (Reader)

type WithParent a = A.Document -> Maybe A.Header -> a

noParent :: a -> WithParent a
noParent a _ _ = a

data BlockStackZipper = BSZ
  { _bszLevel :: Int,
    _bszAttr :: A.Attr,
    _bszTitle :: Text,
    _bszRefTo :: Set Id,
    _bszLeft :: [WithParent A.Block],
    _bszParent :: Maybe BlockStackZipper
  }

makeLenses ''BlockStackZipper

data ParseState = ParseState
  { _blks :: [Block],
    _stack :: BlockStackZipper
  }

makeLenses ''ParseState

type ParseM = State ParseState

getBlock :: ParseM (Maybe Block)
getBlock = blks %%= (listToMaybe &&& tail)

pushBlock :: WithParent A.Block -> ParseM ()
pushBlock blk = stack . bszLeft %= (blk :)

pushHeader :: Int -> A.Attr -> ParseM ()
pushHeader lvl attr = stack %= BSZ lvl attr "" S.empty [] . Just

access :: Getting a ParseState a -> ParseM a
access l = get <&> (^. l)

headerLvl :: ParseM Int
headerLvl = access $ stack . bszLevel

bszToHeader :: BlockStackZipper -> WithParent A.Header
bszToHeader (BSZ lvl attr title ref bks _) doc parent =
  let hd =
        A.Header
          { A._hdAttr = attr,
            A._hdTitle = title,
            A._hdRefTo = ref,
            A._hdLevel = lvl,
            A._hdContent = bks <&> \blk -> blk doc (Just hd),
            A._hdParent = parent,
            A._hdDocument = doc
          }
   in hd

refTo :: Id -> ParseM ()
refTo i = stack . bszRefTo %= S.insert i

popHeader :: ParseM ()
popHeader = do
  bsz <- access stack
  case bsz ^. bszParent of
    Just parent -> do
      stack .= parent
      stack . bszRefTo %= S.union (bsz ^. bszRefTo)
      pushBlock $ \doc hd -> A.Sub (bszToHeader bsz doc hd)
    Nothing -> pure ()

startHeader :: Int -> A.Attr -> ParseM ()
startHeader lvl attr = do
  whileM_ (headerLvl <&> (>= lvl)) popHeader
  pushHeader lvl attr

emptyAttr :: A.Attr
emptyAttr = A.MkAttr "" [] M.empty

run :: ParseM () -> Metadata -> [Block] -> A.Document
run act mtdt bks =
  let doc =
        A.Document
          { A._docMtdt = mtdt,
            A._docContent = st ^. stack . bszLeft <&> \bk -> bk doc Nothing,
            A._docTitle = st ^. stack . bszTitle,
            A._docRefTo = st ^. stack . bszRefTo
          }
   in doc
  where
    st =
      execState (act >> whileM_ (headerLvl <&> (> 0)) popHeader) $
        ParseState bks (BSZ 0 emptyAttr "" S.empty [] Nothing)

parsePandoc :: Pandoc -> A.Document
parsePandoc (Pandoc mtdt bks) = run act (M.map toJSON $ unMeta mtdt) bks
  where
    act = do
      title <- concatMapM parseInline $ docTitle mtdt
      stack . bszTitle .= renderInlines title
      whileJust_ getBlock parseTopBlock

parseTopBlock :: Block -> ParseM ()
parseTopBlock (Header lvl attr title) = do
  let parsed = parseAttr attr
  startHeader lvl parsed
  rendered <- renderInlines <$> concatMapM parseInline title
  stack . bszTitle .= rendered
parseTopBlock bk = mapM_ (pushBlock . noParent) =<< parseBlock bk

-- Parse a block that is not a header
parseBlock :: Block -> ParseM [A.Block]
parseBlock (Plain inls) = pure . A.Para <$> concatMapM parseInline inls
parseBlock (Para inls) = pure . A.Para <$> concatMapM parseInline inls
parseBlock (LineBlock lns) = pure . A.LineBlock <$> mapM (concatMapM parseInline) lns
parseBlock (CodeBlock attr txt) = pure . pure $ A.CodeBlock (parseAttr attr) txt
parseBlock (RawBlock _ _) = pure []
parseBlock (BlockQuote bks) = pure . A.BlockQuote <$> concatMapM parseBlock bks
parseBlock HorizontalRule = pure []
parseBlock (Table attr caption spec hd body ft) = undefined
parseBlock (Figure attr caption bks) = undefined
parseBlock _ = pure []

parseInline :: Inline -> ParseM [A.Inline]
parseInline (Str txt) = pure . pure $ A.Plain txt
parseInline (Emph inls) = pure . A.Styled A.Emph <$> concatMapM parseInline inls
parseInline (Underline inls) = concatMapM parseInline inls
parseInline (Strong inls) = pure . A.Styled A.Emph <$> concatMapM parseInline inls
parseInline (Strikeout inls) = concatMapM parseInline inls
parseInline (Subscript inls) = concatMapM parseInline inls
parseInline (Superscript inls) = concatMapM parseInline inls
parseInline (SmallCaps inls) = concatMapM parseInline inls
parseInline (Quoted _ inls) = pure . A.Styled A.Quote <$> concatMapM parseInline inls
parseInline (Cite cts _) = pure $ A.Cite . MkId . citationId <$> cts
parseInline (Code attr cd) = pure . pure $ A.Code (parseAttr attr) cd
parseInline Space = pure $ pure A.Space
parseInline SoftBreak = pure $ pure A.Break
parseInline LineBreak = pure $ pure A.Break
parseInline (Math DisplayMath mth) = pure . pure $ A.DisplayMath mth
parseInline (Math InlineMath mth) = pure . pure $ A.InlineMath mth
parseInline (RawInline _ _) = pure []
parseInline (Link attr txt (url, _)) =
  if T.isInfixOf "://" url
    then pure . pure $ A.PlainLink $ fromMaybe nullURI $ parseURI $ T.unpack url
    else do
      let i = MkId url
      refTo i
      t <- concatMapM parseInline txt
      pure . pure $ A.Link (parseAttr attr) t i
parseInline (Image attr txt url) = parseInline $ Link attr txt url
parseInline (Note bks) = pure . A.Sidenote <$> concatMapM parseBlock bks
parseInline (Span _ inls) = concatMapM parseInline inls

parseAttr :: Attr -> A.Attr
parseAttr (i, cls, mtdt) = A.MkAttr i cls $ M.fromList mtdt
