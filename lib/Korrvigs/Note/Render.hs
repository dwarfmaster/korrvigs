module Korrvigs.Note.Render (writeNote, writeNoteLazy, writeHeaderLazy) where

import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Text (encodeToLazyText)
import Data.ByteString.Lazy (hPutStr)
import qualified Data.ByteString.Lazy as BSL
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Vector as V
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry.Ident
import Korrvigs.Metadata.Task
import Korrvigs.Note.AST
import Korrvigs.Note.Helpers (renderInlines)
import Korrvigs.Note.Render.Monad
import Korrvigs.Note.Render.Table
import Korrvigs.Utils.Crypto
import System.IO hiding (hPutStr)
import Prelude hiding (break)

flushNotes :: RenderM ()
flushNotes = do
  nts <- use notes
  separatedRenders 2 $ for (reverse nts) $ \(i, note) -> do
    writeText . T.pack $ "[^" <> show i <> "]:"
    withoutBreak flush
    withPrefix "  " $ separatedBks 2 note
  notes .= []

flushLinks :: RenderM ()
flushLinks = do
  lks <- use links
  separatedRenders 1 $ for (reverse lks) $ \(i, link) -> do
    writeText "["
    writeText i
    writeText "]: "
    writeText link
    withoutBreak flush
  links .= []

shouldSpaceList :: [[Block]] -> Bool
shouldSpaceList = any ((>= 2) . length)

withListMarker :: (RenderM () -> RenderM a) -> RenderM a
withListMarker rdr = do
  d <- use listDepth
  let marker = case d `mod` 3 of
        0 -> '-'
        1 -> '+'
        _ -> '*'
  listDepth += 1
  r <- rdr $ writeText $ T.singleton marker
  listDepth .= d
  pure r

writeNote :: (MonadIO m) => Handle -> Document -> m (Maybe Text)
writeNote file doc = do
  let txt = writeNoteLazy doc
  liftIO $ do
    r <- try $ hPutStr file txt :: IO (Either SomeException ())
    case r of
      Left e -> pure . Just . T.pack $ "IO Error: " <> show e
      Right () -> pure Nothing

writeNoteLazy :: Document -> BSL.ByteString
writeNoteLazy doc = runRenderM 80 (doc ^. docComputations) (render doc)

writeHeaderLazy :: Header -> Map Text (RunnableType, Hash, RunnableResult) -> BSL.ByteString
writeHeaderLazy hd comps = runRenderM 80 comps $ renderBlock $ Sub hd

render :: Document -> RenderM ()
render doc = do
  renderMetadata (doc ^. docTitle) (doc ^. docParents) $ doc ^. docMtdt
  withoutBreak flush
  replicateM_ 2 newline
  renderTopLevel True $ doc ^. docContent
  replicateM_ 2 newline

isSub :: Block -> Bool
isSub (Sub _) = True
isSub _ = False

renderTopLevel :: Bool -> [Block] -> RenderM ()
renderTopLevel nts bks = do
  separatedRenders 2 $ for bks $ \bk -> do
    hasL <- hasLinks
    when (isSub bk && nts && hasL) $ flushLinks >> replicateM_ 2 newline >> flush
    renderBlock bk >> flush
    hasN <- hasNotes
    when (nts && hasN) $ replicateM_ 2 newline >> flushNotes >> flush
  hasL <- hasLinks
  when (nts && hasL) $ replicateM_ 2 newline >> flushLinks >> flush

renderRawText :: Text -> RenderM ()
renderRawText txt =
  let lns = T.lines txt
   in case lns of
        [] -> pure ()
        l1 : ls -> do
          writeText l1
          forM_ ls $ \l -> flush >> newline >> writeText l

separatedBks :: Int -> [Block] -> RenderM ()
separatedBks n = separatedRenders n . map renderBlock

renderBlock :: Block -> RenderM ()
renderBlock (Para inls) = forM_ inls renderInline
renderBlock (LineBlock lns) =
  separatedRenders 1 $ for lns $ \line -> do
    writeText "| "
    withoutBreak $ do
      forM_ line renderInline
      flush
renderBlock (CodeBlock attr code) = do
  let codeTicks = ticksCount $ T.lines code
  writeText (T.replicate codeTicks "`") >> renderAttr attr >> flush >> newline
  renderRawText code >> flush >> newline
  writeText $ T.replicate codeTicks "`"
  mres <- view $ _2 . at (attr ^. attrId)
  forM_ mres $ \(tp, hash, res) -> do
    let lns = T.lines $ encodeToText res
    let ticks = ticksCount lns
    flush >> newline >> newline
    writeText $ T.replicate ticks "`"
    writeText "{=result}" >> flush >> newline
    writeText (attr ^. attrId) >> flush >> newline
    writeText (runTypeName tp) >> flush >> newline
    writeText (digestToText hash) >> flush >> newline
    forM_ lns $ \ln -> writeText ln >> flush >> newline
    writeText $ T.replicate ticks "`"
  where
    updMax :: Text -> Int -> Int
    updMax txt n | T.length txt < n = n
    updMax txt _ | T.all (== '`') txt = T.length txt + 1
    updMax _ n = n
    ticksCount :: [Text] -> Int
    ticksCount = foldr updMax 3
renderBlock (BlockQuote bks) = do
  doPrefix "> " $ separatedBks 2 bks
renderBlock (OrderedList bks) = do
  forM_ (zip [1 ..] bks) $ \(i, bk) -> do
    writeText $ renderNum i
    withoutBreak flush
    withPrefix prefix $ separatedBks 2 bk
    flush
    unless (i == length bks) $ replicateM_ seps newline
  where
    shift :: Int
    shift = (3 +) . floor . logBase (10.0 :: Double) . fromIntegral $ length bks
    prefix :: Text
    prefix = mconcat $ replicate shift " "
    renderNum :: Int -> Text
    renderNum i =
      let num = T.pack $ show i
       in num <> "." <> mconcat (replicate (shift - T.length num - 1) " ")
    seps :: Int
    seps
      | shouldSpaceList bks = 2
      | otherwise = 1
renderBlock (BulletList bks) =
  withListMarker $ \marker -> separatedRenders seps $ for bks $ \bk -> do
    marker >> writeText " " >> withoutBreak flush
    withPrefix "  " $ separatedBks 2 bk
  where
    seps :: Int
    seps
      | shouldSpaceList bks = 2
      | otherwise = 1
renderBlock (DefinitionList defs) =
  separatedRenders 2 $ for defs $ \(term, def) -> do
    withoutBreak $ do
      forM_ term renderInline
      flush
    newline
    separatedRenders seps $ for def $ \bk -> do
      writeText ": " >> withoutBreak flush
      withPrefix "  " $ separatedBks 2 bk
      flush
  where
    bks :: [[Block]]
    bks = defs ^.. each . _2 . each
    seps :: Int
    seps
      | shouldSpaceList bks = 2
      | otherwise = 1
renderBlock (Figure attr caption bks) = withoutBreak $ do
  writeText "!["
  separatedBks 0 caption
  writeText "]("
  separatedBks 0 bks
  writeText ")"
  renderAttr attr
renderBlock (Embed (MkId i)) = do
  writeText "```{=embed}" >> flush >> newline
  writeText i >> flush >> newline
  writeText "```"
renderBlock (EmbedHeader (MkId i)) = do
  writeText "```{=embedhd}" >> flush >> newline
  writeText i >> flush >> newline
  writeText "```"
renderBlock (Collection col nm ids) = do
  writeText "```{=collection}" >> flush >> newline
  renderCollection col nm
  forM_ ids $ \colItem -> renderColItem colItem >> flush >> newline
  writeText "```"
renderBlock (Syndicate nm onlyNew ids) = do
  writeText "```{=syndicate}" >> flush >> newline
  when onlyNew $ writeText "+"
  writeText nm >> flush >> newline
  forM_ ids $ \i -> writeText (unId i) >> flush >> newline
  writeText "```"
renderBlock (Sub header) = do
  writeText $ mconcat $ replicate (header ^. hdLevel) "#"
  writeText " "
  forM_ (header ^. hdTask) $ \task -> do
    writeText "["
    writeText $ task ^. tskStatusName
    writeText "] "
  writeText $ header ^. hdTitle
  writeText " "
  renderAttr $ header ^. hdAttr
  flush
  unless (null $ header ^. hdContent) $ replicateM_ 2 newline
  renderTopLevel True $ header ^. hdContent
renderBlock (Table tbl) = do
  width <- view _1
  comps <- view _2
  renderTable width (\w -> runRenderM w comps . renderTopLevel False) tbl

renderColItem :: CollectionItem -> RenderM ()
renderColItem (ColItemEntry i) = writeText ". " >> writeText (unId i)
renderColItem (ColItemInclude i c) =
  writeText "i " >> writeText (unId i) >> writeText " " >> writeText c
renderColItem (ColItemQuery q) =
  writeText "q " >> writeText (LT.toStrict $ encodeToLazyText q)
renderColItem (ColItemSubOf i) = writeText "s " >> writeText (unId i)
renderColItem (ColItemComment comment) =
  writeText "# " >> writeText comment

renderCollection :: Collection -> Text -> RenderM ()
renderCollection col nm =
  writeText (colTxt <> " " <> nm) >> flush >> newline
  where
    colTxt = case col of
      ColList -> "list"
      ColMap -> "map"
      ColGallery -> "gallery"
      ColTimeline -> "timeline"
      ColNetwork -> "network"
      ColFuzzy -> "fuzzy"
      ColCalendar -> "calendar"
      ColKanban -> "kanban"
      ColTaskList -> "tasklist"
      ColLibrary -> "library"

surrounded :: Text -> RenderM a -> RenderM a
surrounded del act = do
  writeText del
  r <- act
  writeText del
  pure r

renderInline :: Inline -> RenderM ()
renderInline (Plain txt) = writeText txt
renderInline (Styled style inls) = surrounded sym $ forM_ inls renderInline
  where
    sym :: Text
    sym = case style of
      Emph -> "*"
      Quote -> "\""
      SubScript -> "~"
      SuperScript -> "^"
renderInline (Code attr code) = surrounded "`" (writeText code) >> renderAttr attr
renderInline (Link attr txt (MkId i)) = do
  writeText "["
  forM_ txt renderInline
  writeText "](" >> writeText i >> writeText ")"
  renderAttr attr
renderInline (Cite (MkId i)) = writeText "[@" >> writeText i >> writeText "]"
renderInline (MtdtLink Nothing mtdt) = do
  writeText "["
  writeText mtdt
  writeText "](mtdt:"
  writeText mtdt
  writeText ")"
renderInline (MtdtLink (Just title) mtdt) = do
  writeText "["
  forM_ title renderInline
  writeText "](mtdt:"
  writeText mtdt
  writeText ")"
renderInline (PlainLink Nothing uri) =
  writeText "<" >> writeText (T.pack $ show uri) >> writeText ">"
renderInline (PlainLink (Just title) uri) = do
  let rtitle = renderInlines title
  if T.unpack rtitle == show uri
    then do
      writeText "<"
      writeText (T.pack $ show uri)
      writeText ">"
    else do
      writeText "["
      forM_ title renderInline
      writeText "]["
      key <- registerLink (renderInlines title) $ T.pack $ show uri
      writeText key
      writeText "]"
renderInline Space = flush
renderInline Break = flush
renderInline (DisplayMath mth) = surrounded "$$" $ writeText mth
renderInline (InlineMath mth) = surrounded "$" $ writeText mth
renderInline (Sidenote note) = do
  num <- registerNote note
  writeText . T.pack $ "[^" <> show num <> "]"
renderInline (Check TaskTodo) = writeText "[ ]"
renderInline (Check TaskImportant) = writeText "[!]"
renderInline (Check TaskOngoing) = writeText "[*]"
renderInline (Check TaskBlocked) = writeText "[-]"
renderInline (Check TaskDone) = writeText "[x]"
renderInline (Check TaskDont) = writeText "[X]"

renderAttr :: Attr -> RenderM ()
renderAttr attr = listOnLine as (writeText "{") (writeText " ") (writeText "}")
  where
    i = [writeText "#" >> writeText (attr ^. attrId) | not . T.null $ attr ^. attrId]
    cls = [writeText "." >> writeText c | c <- attr ^. attrClasses]
    attributes = [writeText key >> writeText "=\"" >> writeText value >> writeText "\"" | (key, value) <- M.toList (attr ^. attrMtdt)]
    as = i ++ cls ++ attributes

renderMetadata :: Text -> Set Id -> Map (CI Text) Value -> RenderM ()
renderMetadata title parents mtdt = withoutBreak $ do
  writeText "---" >> flush >> newline
  writeText "title: " >> renderToYAML True (String title) >> flush >> newline
  unless (S.null parents) $ do
    writeText "parents: " >> flush
    withPrefix "  " $ renderToYAML True $ toJSON $ unId <$> S.toList parents
    flush >> newline
  forM_ (M.toList mtdt) $ \(key, val) -> do
    writeText (CI.foldedCase key) >> writeText ": " >> flush
    withPrefix "  " $ renderToYAML True val
    flush >> newline
  writeText "..." >> flush

renderToYAML :: Bool -> Value -> RenderM ()
renderToYAML _ Null = writeText "~"
renderToYAML _ (Bool True) = writeText "true"
renderToYAML _ (Bool False) = writeText "false"
renderToYAML _ (Number num) = writeText . (<> "\"") . ("\"n:" <>) . T.pack $ show num
renderToYAML _ (String txt) =
  surrounded "\"" $ writeText $ T.replace "\"" "\\\"" $ T.replace "\\" "\\\\" written
  where
    written = if T.isPrefixOf "n:" txt || T.isPrefixOf "t:" txt then "t:" <> txt else txt
renderToYAML _ (Array vals) | V.null vals = writeText "[]"
renderToYAML _ (Array vals) = do
  newline
  separatedRenders 1 $ for (V.toList vals) $ \v -> do
    writeText "-" >> flush
    withPrefix "  " $ renderToYAML False v
renderToYAML po (Object o) = do
  when po newline
  separatedRenders 1 $ for (KM.toList o) $ \(k, v) -> do
    writeText (K.toText k) >> writeText ":" >> flush
    withPrefix "  " (renderToYAML True v) >> flush
