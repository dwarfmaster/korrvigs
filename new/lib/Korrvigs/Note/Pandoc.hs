module Korrvigs.Note.Pandoc (parsePandoc) where

import Control.Arrow ((&&&))
import Control.Lens hiding ((<|))
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

pushHeader :: Int -> A.Attr -> Text -> ParseM ()
pushHeader lvl attr title = stack %= BSZ lvl attr title S.empty [] . Just

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

popHeader :: ParseM ()
popHeader = do
  bsz <- access stack
  case bsz ^. bszParent of
    Just parent -> do
      stack .= parent
      pushBlock $ \doc hd -> A.Sub (bszToHeader bsz doc hd)
    Nothing -> pure ()

startHeader :: Int -> A.Attr -> Text -> ParseM ()
startHeader lvl attr title = do
  whileM_ (headerLvl <&> (>= lvl)) popHeader
  pushHeader lvl attr title

emptyAttr :: A.Attr
emptyAttr = A.MkAttr "" [] M.empty

run :: ParseM () -> Metadata -> [Block] -> Text -> A.Document
run act mtdt bks title =
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
        ParseState bks (BSZ 0 emptyAttr title S.empty [] Nothing)

parsePandoc :: Pandoc -> A.Document
parsePandoc (Pandoc mtdt bks) =
  run (whileJust_ getBlock parseTopBlock) (M.map toJSON $ unMeta mtdt) bks $
    renderInlines $
      parseInline =<< docTitle mtdt

parseTopBlock :: Block -> ParseM ()
parseTopBlock (Header lvl attr title) = startHeader lvl parsed rendered
  where
    parsed = parseAttr attr
    rendered = renderInlines $ parseInline =<< title
parseTopBlock bk = mapM_ (pushBlock . noParent) $ parseBlock bk

-- Parse a block that is not a header
parseBlock :: Block -> [A.Block]
parseBlock (Plain inls) = pure $ A.Para $ parseInline =<< inls
parseBlock (Para inls) = pure $ A.Para $ parseInline =<< inls
parseBlock (LineBlock lns) = pure $ A.LineBlock $ (parseInline =<<) <$> lns
parseBlock (CodeBlock attr txt) = pure $ A.CodeBlock (parseAttr attr) txt
parseBlock (RawBlock _ _) = []
parseBlock (BlockQuote bks) = pure $ A.BlockQuote $ parseBlock =<< bks
parseBlock HorizontalRule = []
parseBlock (Table attr caption spec hd body ft) = undefined
parseBlock (Figure attr caption bks) = undefined
parseBlock _ = []

parseInline :: Inline -> [A.Inline]
parseInline (Str txt) = pure $ A.Plain txt
parseInline (Emph inls) = pure $ A.Styled A.Emph $ parseInline =<< inls
parseInline (Underline inls) = parseInline =<< inls
parseInline (Strong inls) = pure $ A.Styled A.Emph $ parseInline =<< inls
parseInline (Strikeout inls) = parseInline =<< inls
parseInline (Subscript inls) = parseInline =<< inls
parseInline (Superscript inls) = parseInline =<< inls
parseInline (SmallCaps inls) = parseInline =<< inls
parseInline (Quoted _ inls) = pure $ A.Styled A.Quote $ parseInline =<< inls
parseInline (Cite cts _) = A.Cite . MkId . citationId <$> cts
parseInline (Code attr cd) = pure $ A.Code (parseAttr attr) cd
parseInline Space = pure A.Space
parseInline SoftBreak = pure A.Break
parseInline LineBreak = pure A.Break
parseInline (Math DisplayMath mth) = pure $ A.DisplayMath mth
parseInline (Math InlineMath mth) = pure $ A.InlineMath mth
parseInline (RawInline _ _) = []
parseInline (Link attr txt (url, _)) =
  if T.isInfixOf "://" url
    then pure $ A.PlainLink $ fromMaybe nullURI $ parseURI $ T.unpack url
    else pure $ A.Link (parseAttr attr) (parseInline =<< txt) $ MkId url
parseInline (Image attr txt url) = parseInline $ Link attr txt url
parseInline (Note bks) = pure $ A.Sidenote $ parseBlock =<< bks
parseInline (Span _ inls) = parseInline =<< inls

parseAttr :: Attr -> A.Attr
parseAttr (i, cls, mtdt) = A.MkAttr i cls $ M.fromList mtdt
