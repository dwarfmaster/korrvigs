module Korrvigs.Note.Pandoc (readNote, readNoteFromText, parsePandoc, parseTopBlocks) where

import Control.Arrow (first, (&&&))
import Control.Exception (SomeException, try)
import Control.Lens hiding ((<|))
import Control.Monad
import Control.Monad.Extra (concatMapM)
import Control.Monad.Loops (iterateWhile, whileJust_, whileM_)
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Aeson hiding ((.=))
import Data.Array.Base hiding (array)
import qualified Data.Array.ST as SAr
import Data.Bitraversable (bimapM)
import qualified Data.CaseInsensitive as CI
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.STRef
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc
import Korrvigs.Entry.Ident
import Korrvigs.Metadata.Task
import qualified Korrvigs.Note.AST as A
import Korrvigs.Note.Helpers
import Korrvigs.Utils.JSON (jsonAsText)
import Korrvigs.Utils.Pandoc
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
    _bszTask :: Maybe Task,
    _bszRefTo :: Set Id,
    _bszChecks :: A.Checks,
    _bszLeft :: [WithParent A.Block],
    _bszTasks :: [Task],
    _bszCollections :: Set Text,
    _bszNamedSubs :: Set Text,
    _bszNamedCode :: Set Text,
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
getBlock = blks %%= (listToMaybe &&& drop 1)

pushBlock :: WithParent A.Block -> ParseM ()
pushBlock blk = stack . bszLeft %= (blk :)

pushHeader :: Int -> A.Attr -> ParseM ()
pushHeader lvl attr = stack %= BSZ lvl attr "" Nothing S.empty def [] [] S.empty S.empty S.empty . Just

headerLvl :: ParseM Int
headerLvl = use $ stack . bszLevel

bszToHeader :: BlockStackZipper -> WithParent A.Header
bszToHeader bsz doc parent =
  let hd =
        A.Header
          { A._hdAttr = bsz ^. bszAttr,
            A._hdTitle = bsz ^. bszTitle,
            A._hdRefTo = bsz ^. bszRefTo,
            A._hdTask = bsz ^. bszTask,
            A._hdTasks = bsz ^. bszTasks,
            A._hdChecks = bsz ^. bszChecks,
            A._hdLevel = bsz ^. bszLevel,
            A._hdContent = reverse $ (bsz ^. bszLeft) <&> \blk -> blk doc (Just hd),
            A._hdParent = parent,
            A._hdDocument = doc,
            A._hdCollections = bsz ^. bszCollections
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

propagateChecks :: BlockStackZipper -> ParseM ()
propagateChecks bsz = case bsz ^. bszTask of
  Just task -> do
    let st = task ^. tskStatus
    when (st == TaskTodo) $ stack . bszChecks . A.ckTodo %= (+ 1)
    when (st == TaskImportant) $ stack . bszChecks . A.ckImportant %= (+ 1)
    when (st == TaskOngoing) $ stack . bszChecks . A.ckOngoing %= (+ 1)
    when (st == TaskBlocked) $ stack . bszChecks . A.ckBlocked %= (+ 1)
    when (st == TaskDone) $ stack . bszChecks . A.ckDone %= (+ 1)
    when (st == TaskDont) $ stack . bszChecks . A.ckDont %= (+ 1)
  Nothing -> do
    stack . bszChecks . A.ckTodo %= (bsz ^. bszChecks . A.ckTodo +)
    stack . bszChecks . A.ckImportant %= (bsz ^. bszChecks . A.ckImportant +)
    stack . bszChecks . A.ckOngoing %= (bsz ^. bszChecks . A.ckOngoing +)
    stack . bszChecks . A.ckBlocked %= (bsz ^. bszChecks . A.ckBlocked +)
    stack . bszChecks . A.ckDone %= (bsz ^. bszChecks . A.ckDone +)
    stack . bszChecks . A.ckDont %= (bsz ^. bszChecks . A.ckDont +)

popHeader :: ParseM Bool
popHeader = do
  bsz <- use stack
  case bsz ^. bszParent of
    Just parent -> do
      stack .= parent
      stack . bszRefTo %= S.union (bsz ^. bszRefTo)
      stack . bszTasks %= (++ (toList (bsz ^. bszTask) ++ bsz ^. bszTasks))
      stack . bszCollections %= S.union (bsz ^. bszCollections)
      propagateChecks bsz
      pushBlock $ \doc hd -> A.Sub (bszToHeader bsz doc hd)
      pure True
    Nothing -> pure False

startHeader :: Int -> A.Attr -> ParseM ()
startHeader lvl attr = do
  whileM_ (headerLvl <&> (>= lvl)) popHeader
  pushHeader lvl attr

emptyAttr :: A.Attr
emptyAttr = A.MkAttr "" [] M.empty

run :: ParseM () -> Map Text Value -> [Block] -> A.Document
run act mtdt bks =
  let doc =
        A.Document
          { A._docMtdt = M.delete (CI.mk "parents") cimtdt,
            A._docContent = reverse $ st ^. stack . bszLeft <&> \bk -> bk doc Nothing,
            A._docTitle = st ^. stack . bszTitle,
            A._docRefTo = st ^. stack . bszRefTo,
            A._docTask = st ^. stack . bszTask,
            A._docTasks = st ^. stack . bszTasks,
            A._docChecks = st ^. stack . bszChecks,
            A._docParents = S.fromList $ fmap MkId $ join $ toList $ maybe (Success []) fromJSON $ M.lookup (CI.mk "parents") cimtdt,
            A._docCollections = st ^. stack . bszCollections,
            A._docNamedSubs = st ^. stack . bszNamedSubs,
            A._docNamedCode = st ^. stack . bszNamedCode
          }
   in doc
  where
    cimtdt = M.fromList $ first CI.mk <$> M.toList mtdt
    st =
      execState (act >> iterateWhile id popHeader) $
        ParseState bks (BSZ 0 emptyAttr "" Nothing S.empty def [] [] S.empty S.empty S.empty Nothing)

readNote :: (MonadIO m) => FilePath -> m (Either Text A.Document)
readNote pth = liftIO $ do
  file <- try $ readFile pth :: IO (Either SomeException Text)
  case file of
    Left e -> pure . Left . T.pack $ "IO error: " <> show e
    Right f -> readNoteFromText parsePandoc f

readNoteFromText :: (MonadIO m) => (Pandoc -> a) -> Text -> m (Either Text a)
readNoteFromText reader txt =
  liftIO (runIO $ readMarkdown readerOptions txt) <&> \case
    Left e -> Left $ "Pandoc error: " <> renderError e
    Right pandoc -> Right $ reader pandoc
  where
    readerOptions :: ReaderOptions
    readerOptions =
      def
        { readerExtensions = disableExtension Ext_auto_identifiers $ disableExtension Ext_task_lists pandocExtensions
        }

parsePandoc :: Pandoc -> A.Document
parsePandoc (Pandoc mtdt bks) = run act meta bks
  where
    meta = M.map parseMetaValue $ unMeta mtdt
    act = do
      title <- parseInlines $ docTitle mtdt
      let rendered = renderInlines title
      stack . bszTitle .= rendered
      forM_ (M.lookup "task" meta >>= jsonAsText) $ \stname -> do
        let st = fromMaybe TaskTodo $ parseStatusName stname
        let tsk = mkTask st stname & tskLabel .~ rendered
        stack . bszTask ?= applyTaskMtdt (flip M.lookup meta . CI.foldedCase) tsk
      whileJust_ getBlock parseTopBlock

parseTopBlocks :: Int -> Pandoc -> [A.Block]
parseTopBlocks lvl (Pandoc _ bks) = run act M.empty bks ^. A.docContent
  where
    act = do
      stack . bszLevel .= lvl - 1
      whileJust_ getBlock parseTopBlock

parseTopBlock :: Block -> ParseM ()
parseTopBlock (Header lvl attr title) = do
  let parsed = parseAttr attr
  let i = parsed ^. A.attrId
  unless (T.null i) $ stack . bszNamedSubs %= S.insert i
  startHeader lvl parsed
  rendered <- renderInlines <$> parseTitleInlines title
  stack . bszTitle .= rendered
  isTask <- isJust <$> use (stack . bszTask)
  when isTask $ do
    stack . bszTask . _Just . tskLabel .= rendered
    let look = fmap toJSON . flip M.lookup (parsed ^. A.attrMtdt) . CI.foldedCase
    stack . bszTask . _Just %= applyTaskMtdt look
parseTopBlock bk = mapM_ (pushBlock . noParent) =<< parseBlock bk

-- Parse a block that is not a header
parseBlock :: Block -> ParseM [A.Block]
parseBlock (Plain inls) = pure . A.Para <$> parseInlines inls
parseBlock (Para inls) = pure . A.Para <$> parseInlines inls
parseBlock (LineBlock lns) = pure . A.LineBlock <$> mapM parseInlines lns
parseBlock (CodeBlock attr txt) = do
  let nattr = parseAttr attr
  let i = nattr ^. A.attrId
  unless (T.null i) $ stack . bszNamedCode %= S.insert i
  pure . pure $ A.CodeBlock nattr txt
parseBlock (RawBlock (Format fmt) i)
  | CI.mk fmt == "embed" = do
      refTo $ MkId i
      pure . pure . A.Embed . MkId $ i
  | CI.mk fmt == "embedhd" = do
      refTo $ MkId i
      pure . pure . A.EmbedHeader . MkId $ i
  | CI.mk fmt == "collection" = case T.lines i of
      [] -> pure $ pure $ A.Collection A.ColList "TODO" []
      (hd : ids) -> do
        (col, colname) <- parseColName hd
        stack . bszCollections %= S.insert colname
        let items = parseColItem <$> ids
        let directItems = mapMaybe extractItem items
        forM_ directItems refTo
        pure . pure . A.Collection col colname $ items
parseBlock (RawBlock _ _) = pure []
parseBlock (BlockQuote bks) = pure . A.BlockQuote <$> concatMapM parseBlock bks
parseBlock (OrderedList _ bks) =
  pure . A.OrderedList <$> mapM (concatMapM parseBlock) bks
parseBlock (BulletList bks) =
  pure . A.BulletList <$> mapM (concatMapM parseBlock) bks
parseBlock (DefinitionList lst) =
  pure . A.DefinitionList
    <$> mapM (bimapM parseInlines (mapM (concatMapM parseBlock))) lst
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

parseColItem :: Text -> A.CollectionItem
parseColItem line = case prefix of
  ". " -> A.ColItemEntry $ suffixId suffix
  "i " ->
    let (i, col) = T.break (== ' ') suffix in A.ColItemInclude (MkId i) (T.strip col)
  "q " -> case eitherDecode (LEnc.encodeUtf8 $ LT.fromStrict suffix) of
    Left err -> A.ColItemComment $ suffix <> ": " <> T.pack err
    Right q -> A.ColItemQuery q
  "s " -> A.ColItemSubOf $ suffixId suffix
  "# " -> A.ColItemComment suffix
  _ -> A.ColItemComment line
  where
    (prefix, suffix) = T.splitAt 2 line
    suffixId = MkId . T.strip

extractItem :: A.CollectionItem -> Maybe Id
extractItem (A.ColItemEntry i) = Just i
extractItem _ = Nothing

mkTask :: TaskStatus -> Text -> Task
mkTask st stname = Task st stname "" Nothing Nothing Nothing Nothing

matchStatus :: Text -> Maybe TaskStatus
matchStatus txt
  | not (T.null txt) && T.head txt == '[' && T.last txt == ']' =
      Just $ fromMaybe TaskTodo $ parseStatusName $ T.init $ T.tail txt
matchStatus _ = Nothing

parseColName :: Text -> ParseM (A.Collection, Text)
parseColName hd = do
  let (coltype, rawColname) = T.break (== ' ') hd
  let colname = T.dropWhile (== ' ') rawColname
  col <- case coltype of
    "list" -> pure A.ColList
    "map" -> pure A.ColMap
    "gallery" -> pure A.ColGallery
    "timeline" -> pure A.ColTimeline
    "network" -> pure A.ColNetwork
    "fuzzy" -> pure A.ColFuzzy
    "calendar" -> pure A.ColCalendar
    "kanban" -> pure A.ColKanban
    "biblio" -> pure A.ColBiblio
    "tasklist" -> pure A.ColTaskList
    _ -> pure A.ColList
  pure (col, colname)

parseTitleInlines :: [Inline] -> ParseM [A.Inline]
parseTitleInlines (Str stname : xs) = case matchStatus stname of
  Just st -> do
    stack . bszTask ?= mkTask st (T.init $ T.tail stname)
    parseTitleInlines xs
  Nothing -> concatMapM parseInline $ Str stname : xs
parseTitleInlines xs = concatMapM parseInline xs

parseInlines :: [Inline] -> ParseM [A.Inline]
parseInlines (Str "[x]" : xs) = do
  stack . bszChecks . A.ckDone %= (+ 1)
  (A.Check TaskDone :) <$> parseInlines xs
parseInlines (Str "[*]" : xs) = do
  stack . bszChecks . A.ckOngoing %= (+ 1)
  (A.Check TaskOngoing :) <$> parseInlines xs
parseInlines (Str "[-]" : xs) = do
  stack . bszChecks . A.ckBlocked %= (+ 1)
  (A.Check TaskBlocked :) <$> parseInlines xs
parseInlines (Str "[X]" : xs) = do
  stack . bszChecks . A.ckDont %= (+ 1)
  (A.Check TaskDont :) <$> parseInlines xs
parseInlines (Str "[!]" : xs) = do
  stack . bszChecks . A.ckImportant %= (+ 1)
  (A.Check TaskImportant :) <$> parseInlines xs
parseInlines (Str "[" : Space : Str "]" : xs) = do
  stack . bszChecks . A.ckTodo %= (+ 1)
  (A.Check TaskTodo :) <$> parseInlines xs
parseInlines (x : xs) = (++) <$> parseInline x <*> parseInlines xs
parseInlines [] = pure []

parseInline :: Inline -> ParseM [A.Inline]
parseInline (Str txt) = pure . pure $ A.Plain txt
parseInline (Emph inls) = pure . A.Styled A.Emph <$> parseInlines inls
parseInline (Underline inls) = parseInlines inls
parseInline (Strong inls) = pure . A.Styled A.Emph <$> parseInlines inls
parseInline (Strikeout inls) = parseInlines inls
parseInline (Subscript inls) =
  pure . A.Styled A.SubScript <$> parseInlines inls
parseInline (Superscript inls) =
  pure . A.Styled A.SuperScript <$> parseInlines inls
parseInline (SmallCaps inls) = parseInlines inls
parseInline (Quoted _ inls) = pure . A.Styled A.Quote <$> parseInlines inls
parseInline (Cite cts _) = pure $ A.Cite . MkId . citationId <$> cts
parseInline (Code attr cd) = pure . pure $ A.Code (parseAttr attr) cd
parseInline Space = pure $ pure A.Space
parseInline SoftBreak = pure $ pure A.Space
parseInline LineBreak = pure $ pure A.Break
parseInline (Math DisplayMath mth) = pure . pure $ A.DisplayMath mth
parseInline (Math InlineMath mth) = pure . pure $ A.InlineMath mth
parseInline (RawInline _ _) = pure []
parseInline (Link attr txt (url, _)) = do
  title <- parseInlines txt
  if T.isInfixOf "://" url
    then do
      let mtitle = if null txt then Nothing else Just title
      pure . pure $ A.PlainLink mtitle $ fromMaybe nullURI $ parseURI $ T.unpack url
    else do
      let i = MkId url
      refTo i
      pure . pure $ A.Link (parseAttr attr) title i
parseInline (Image attr txt url) = parseInline $ Link attr txt url
parseInline (Note bks) = pure . A.Sidenote <$> concatMapM parseBlock bks
parseInline (Span _ inls) = parseInlines inls

parseAttr :: Attr -> A.Attr
parseAttr (i, cls, mtdt) = A.MkAttr i cls $ M.fromList mtdt

-- Tables
tableSize :: [Row] -> (Int, Int)
tableSize rows = (maybe 0 width $ listToMaybe rows, height)
  where
    rowHeight :: Int -> Row -> Int
    rowHeight y (Row _ cells) = mmax $ map (\(Cell _ _ (RowSpan h) _ _) -> y + h) cells
    height :: Int
    height = mmax $ uncurry rowHeight <$> zip [0 ..] rows
    width :: Row -> Int
    width (Row _ cells) = sum $ map (\(Cell _ _ _ (ColSpan w) _) -> w) cells
    mmax :: [Int] -> Int
    mmax [] = 0
    mmax l = maximum l

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
