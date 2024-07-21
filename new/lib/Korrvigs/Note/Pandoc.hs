module Korrvigs.Note.Pandoc (readNote) where

import Control.Arrow ((&&&))
import Control.Exception (SomeException, try)
import Control.Lens hiding ((<|))
import Control.Monad.Extra (concatMapM)
import Control.Monad.Loops (whileJust_, whileM_)
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Aeson hiding ((.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Array.Base hiding (array)
import qualified Data.Array.ST as SAr
import Data.Bitraversable (bimapM)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Maybe
import Data.STRef
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder
import qualified Data.Vector as V
import Korrvigs.Entry (Metadata)
import Korrvigs.Entry.Ident
import qualified Korrvigs.Note.AST as A
import Korrvigs.Note.Helpers
import Network.URI
import Text.Pandoc hiding (Reader)
import Prelude hiding (readFile)

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
            A._hdContent = reverse $ bks <&> \blk -> blk doc (Just hd),
            A._hdParent = parent,
            A._hdDocument = doc
          }
   in hd

refTo :: Id -> ParseM ()
refTo i = stack . bszRefTo %= S.insert i

data BuildingCell = BCell
  { _bcOrig :: (Int, Int),
    _bcWidth :: Int,
    _bcHeight :: Int,
    _bcData :: [Block]
  }

makeLenses ''BuildingCell

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
            A._docContent = reverse $ st ^. stack . bszLeft <&> \bk -> bk doc Nothing,
            A._docTitle = st ^. stack . bszTitle,
            A._docRefTo = st ^. stack . bszRefTo
          }
   in doc
  where
    st =
      execState (act >> whileM_ (headerLvl <&> (> 0)) popHeader) $
        ParseState bks (BSZ 0 emptyAttr "" S.empty [] Nothing)

readNote :: (MonadIO m) => FilePath -> m (Either Text A.Document)
readNote pth = liftIO $ do
  file <- try $ readFile pth :: IO (Either SomeException Text)
  case file of
    Left e -> pure . Left . T.pack $ "IO error: " <> show e
    Right f -> do
      doRead <- runIO $ readMarkdown readerOptions f
      case doRead of
        Left e -> pure . Left $ "Pandoc error: " <> renderError e
        Right pandoc -> pure . Right $ parsePandoc pandoc
  where
    readerOptions :: ReaderOptions
    readerOptions =
      def
        { readerExtensions = pandocExtensions
        }

parsePandoc :: Pandoc -> A.Document
parsePandoc (Pandoc mtdt bks) = run act (M.map parseMetaValue $ unMeta mtdt) bks
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
parseBlock (RawBlock (Format "Embed") i) = pure . pure . A.Embed . MkId $ i
parseBlock (RawBlock _ _) = pure []
parseBlock (BlockQuote bks) = pure . A.BlockQuote <$> concatMapM parseBlock bks
parseBlock (OrderedList _ bks) =
  pure . A.OrderedList <$> mapM (concatMapM parseBlock) bks
parseBlock (BulletList bks) =
  pure . A.BulletList <$> mapM (concatMapM parseBlock) bks
parseBlock (DefinitionList lst) =
  pure . A.DefinitionList
    <$> mapM (bimapM (concatMapM parseInline) (mapM (concatMapM parseBlock))) lst
parseBlock HorizontalRule = pure []
parseBlock (Table attr (Caption _ caption) _ (TableHead _ headR) body (TableFoot _ footR)) = do
  let a = parseAttr attr
  capt <- concatMapM parseBlock caption
  let bodyR = concatMap (\(TableBody _ _ _ rows) -> rows) body
  let headN = tableSize headR ^. _2
  let (width, height) = tableSize bodyR
  let footN = tableSize footR ^. _2
  cells <- mapM buildCell $ SAr.runSTArray $ buildCells width (headN + height + footN) $ join [headR, bodyR, footR]
  pure . pure . A.Table $
    A.MkTable
      { A._tableCaption = capt,
        A._tableCells = cells,
        A._tableAttr = a,
        A._tableHeader = headN,
        A._tableFooter = footN
      }
parseBlock (Figure attr (Caption _ caption) bks) = do
  capt <- concatMapM parseBlock caption
  content <- concatMapM parseBlock bks
  let a = parseAttr attr
  pure . pure $ A.Figure a capt content
parseBlock _ = pure []

parseInline :: Inline -> ParseM [A.Inline]
parseInline (Str txt) = pure . pure $ A.Plain txt
parseInline (Emph inls) = pure . A.Styled A.Emph <$> concatMapM parseInline inls
parseInline (Underline inls) = concatMapM parseInline inls
parseInline (Strong inls) = pure . A.Styled A.Emph <$> concatMapM parseInline inls
parseInline (Strikeout inls) = concatMapM parseInline inls
parseInline (Subscript inls) =
  pure . A.Styled A.SubScript <$> concatMapM parseInline inls
parseInline (Superscript inls) =
  pure . A.Styled A.SuperScript <$> concatMapM parseInline inls
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
parseInline (Link attr txt (url, _)) = do
  if T.isInfixOf "://" url
    then do
      pure . pure $ A.PlainLink $ fromMaybe nullURI $ parseURI $ T.unpack url
    else do
      let i = MkId url
      refTo i
      title <- concatMapM parseInline txt
      pure . pure $ A.Link (parseAttr attr) title i
parseInline (Image attr txt url) = parseInline $ Link attr txt url
parseInline (Note bks) = pure . A.Sidenote <$> concatMapM parseBlock bks
parseInline (Span _ inls) = concatMapM parseInline inls

parseAttr :: Attr -> A.Attr
parseAttr (i, cls, mtdt) = A.MkAttr i cls $ M.fromList mtdt

-- Tables
tableSize :: [Row] -> (Int, Int)
tableSize rows = (maybe 0 width $ listToMaybe rows, height)
  where
    rowHeight :: Int -> Row -> Int
    rowHeight y (Row _ cells) = maximum $ map (\(Cell _ _ (RowSpan h) _ _) -> y + h) cells
    height :: Int
    height = maximum $ uncurry rowHeight <$> zip [0 ..] rows
    width :: Row -> Int
    width (Row _ cells) = sum $ map (\(Cell _ _ _ (ColSpan w) _) -> w) cells

buildCell :: BuildingCell -> ParseM A.Cell
buildCell bc = do
  dat <- concatMapM parseBlock $ bc ^. bcData
  pure
    A.Cell
      { A._cellOrig = bc ^. bcOrig,
        A._cellWidth = bc ^. bcWidth,
        A._cellHeight = bc ^. bcHeight,
        A._cellData = dat
      }

bcValid :: BuildingCell -> Bool
bcValid bc = bc ^. bcWidth > 0

buildCells :: Int -> Int -> [Row] -> ST s (SAr.STArray s (Int, Int) BuildingCell)
buildCells width height rows = do
  let dcell = BCell (0, 0) 0 0 []
  array <- SAr.newArray ((1, 1), (width, height)) dcell
  forM_ (zip [1 ..] rows) $ \(y, Row _ row) -> do
    x <- newSTRef 1
    forM_ row $ \(Cell _ _ (RowSpan h) (ColSpan w) dat) -> do
      whileM_ (readSTRef x >>= \rx -> bcValid <$> readArray array (rx, y)) $
        modifySTRef x (+ 1)
      rx <- readSTRef x
      let bc =
            BCell
              { _bcOrig = (rx, y),
                _bcWidth = w,
                _bcHeight = h,
                _bcData = dat
              }
      forM_ (SAr.range ((1, 1), (w, h))) $ \(dx, dy) -> do
        writeArray array (rx + dx - 1, y + dy - 1) bc
      writeSTRef x $ rx + w
  pure array

pdInlineToText :: Inline -> Builder
pdInlineToText (Str txt) = fromText txt
pdInlineToText (Emph inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Underline inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Strong inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Strikeout inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Superscript inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Subscript inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (SmallCaps inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Quoted _ inls) = mconcat $ pdInlineToText <$> inls
pdInlineToText (Cite cites _) =
  mconcat . intersperse (fromText ";") $ map (\(Citation i _ _ _ _ _) -> "@" <> fromText i) cites
pdInlineToText (Code _ txt) = fromText txt
pdInlineToText Space = " "
pdInlineToText SoftBreak = " "
pdInlineToText LineBreak = " "
pdInlineToText (Math DisplayMath mth) = "$$" <> fromText mth <> "$$"
pdInlineToText (Math InlineMath mth) = "$" <> fromText mth <> "$"
pdInlineToText (RawInline _ _) = mempty
pdInlineToText (Link _ caption _) = mconcat $ pdInlineToText <$> caption
pdInlineToText (Image _ caption _) = mconcat $ pdInlineToText <$> caption
pdInlineToText (Note _) = mempty
pdInlineToText (Span _ inls) = mconcat $ pdInlineToText <$> inls

pdBlockToText :: Block -> Builder
pdBlockToText (Plain inls) = mconcat $ pdInlineToText <$> inls
pdBlockToText (Para inls) = mconcat $ pdInlineToText <$> inls
pdBlockToText (Div _ bks) = pdBlocksToText bks
pdBlockToText _ = mempty

pdBlocksToText :: [Block] -> Builder
pdBlocksToText bks = mconcat . intersperse " " $ pdBlockToText <$> bks

parseMetaValue :: MetaValue -> Value
parseMetaValue (MetaBool b) = Bool b
parseMetaValue (MetaString txt) = String txt
parseMetaValue (MetaList l) = Array . V.fromList $ parseMetaValue <$> l
parseMetaValue (MetaMap m) =
  Object . KM.fromList $ bimap K.fromText parseMetaValue <$> M.toList m
parseMetaValue (MetaInlines inls) =
  String . toStrict . toLazyText . mconcat $ map pdInlineToText inls
parseMetaValue (MetaBlocks bks) =
  String . toStrict . toLazyText $ pdBlocksToText bks
