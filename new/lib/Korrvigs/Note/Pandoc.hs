module Korrvigs.Note.Pandoc (parsePandoc, toPandoc) where

import Control.Arrow ((&&&))
import Control.Lens hiding ((<|))
import Control.Monad.Extra (concatMapM)
import Control.Monad.Loops (whileJust_, whileM_)
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Aeson hiding (Array, (.=))
import Data.Array (Array)
import Data.Array.Base hiding (array)
import qualified Data.Array.ST as SAr
import qualified Data.Map as M
import Data.Maybe
import Data.STRef
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
parseBlock (RawBlock (Format "Embed") i) = pure . pure . A.Embed . MkId $ i
parseBlock (RawBlock _ _) = pure []
parseBlock (BlockQuote bks) = pure . A.BlockQuote <$> concatMapM parseBlock bks
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
      whileM_ (readSTRef x >>= \rx -> not . bcValid <$> readArray array (rx, y)) $
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
        writeArray array (rx + dx, y + dy) bc
  pure array

-- Generate pandoc
toPandoc :: A.Document -> Pandoc
toPandoc doc = Pandoc mtdt $ toBlock =<< doc ^. A.docContent
  where
    mtdt :: Meta
    mtdt = Meta $ M.map (unwrap . fromJSON) $ doc ^. A.docMtdt
    unwrap :: Result MetaValue -> MetaValue
    unwrap (Error str) = MetaString $ "ERROR(fromJSON): " <> T.pack str
    unwrap (Success v) = v

toBlock :: A.Block -> [Block]
toBlock (A.Para lns) = pure . Para $ toInline =<< lns
toBlock (A.LineBlock llns) = pure . LineBlock $ (toInline =<<) <$> llns
toBlock (A.CodeBlock a code) = pure $ CodeBlock (toAttr a) code
toBlock (A.BlockQuote quote) = pure . BlockQuote $ toBlock =<< quote
toBlock (A.OrderedList bks) =
  pure $ OrderedList (1, DefaultStyle, DefaultDelim) $ (toBlock =<<) <$> bks
toBlock (A.BulletList lst) = pure . BulletList $ (toBlock =<<) <$> lst
toBlock (A.DefinitionList lst) =
  pure . DefinitionList $
    bimap (toInline =<<) (fmap (toBlock =<<)) <$> lst
toBlock (A.Figure a caption bks) =
  pure $ Figure (toAttr a) (Caption Nothing $ toBlock =<< caption) $ toBlock =<< bks
toBlock (A.Embed i) =
  pure $ RawBlock (Format "Embed") $ unId i
toBlock (A.Sub hd) =
  Header (hd ^. A.hdLevel) (toAttr $ hd ^. A.hdAttr) [Str $ hd ^. A.hdTitle]
    : (toBlock =<< hd ^. A.hdContent)
toBlock (A.Table tbl) =
  pure $ Table attr caption colspec header body footer
  where
    attr = toAttr $ tbl ^. A.tableAttr
    caption = Caption Nothing $ toBlock =<< tbl ^. A.tableCaption
    width = bounds (tbl ^. A.tableCells) ^. _2 . _1
    colspec = replicate width (AlignDefault, ColWidthDefault)
    rows = tableToRows $ tbl ^. A.tableCells
    footStart = length rows - tbl ^. A.tableFooter - tbl ^. A.tableHeader
    (headR, withoutHead) = splitAt (tbl ^. A.tableHeader) rows
    (bodyR, footR) = splitAt footStart withoutHead
    header = TableHead pdEmptyAttr headR
    footer = TableFoot pdEmptyAttr footR
    body = [TableBody pdEmptyAttr (RowHeadColumns 0) [] bodyR]

tableToRows :: Array (Int, Int) A.Cell -> [Row]
tableToRows cells =
  flip map (SAr.range (1, height)) $ \y -> Row pdEmptyAttr $
    flip mapMaybe (SAr.range (1, width)) $ \x ->
      let c = cells ! (x, y)
       in if c ^. A.cellOrig == (x, y)
            then Just $ Cell pdEmptyAttr AlignDefault (RowSpan $ c ^. A.cellHeight) (ColSpan $ c ^. A.cellWidth) $ toBlock =<< c ^. A.cellData
            else Nothing
  where
    (_, (width, height)) = bounds cells

pdEmptyAttr :: Attr
pdEmptyAttr = ("", [], [])

toInline :: A.Inline -> [Inline]
toInline (A.Plain p) = pure $ Str p
toInline (A.Styled A.Emph inls) = pure . Emph $ toInline =<< inls
toInline (A.Styled A.Quote inls) = pure $ Quoted DoubleQuote $ toInline =<< inls
toInline (A.Code a code) = pure $ Code (toAttr a) code
toInline (A.Link a inls i) = pure $ Link (toAttr a) (toInline =<< inls) (unId i, "")
toInline (A.Cite i) = pure $ Cite [Citation (unId i) [] [] NormalCitation 0 0] []
toInline (A.PlainLink uri) = pure $ Link pdEmptyAttr [] (T.pack $ show uri, "")
toInline A.Space = pure Space
toInline A.Break = pure SoftBreak
toInline (A.DisplayMath mth) = pure $ Math DisplayMath mth
toInline (A.InlineMath mth) = pure $ Math InlineMath mth
toInline (A.Sidenote content) = pure . Note $ toBlock =<< content

toAttr :: A.Attr -> Attr
toAttr a = (a ^. A.attrId, a ^. A.attrClasses, M.toList $ a ^. A.attrMtdt)
