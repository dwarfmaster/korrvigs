module Korrvigs.Note.Render (writeNote, writeNoteLazy, writeHeaderLazy) where

import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.Aeson (Value (..), toJSON)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
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
import qualified Data.Vector as V
import Korrvigs.Entry.Ident
import Korrvigs.Metadata
import Korrvigs.Note.AST
import Korrvigs.Note.Helpers (renderInlines)
import Korrvigs.Note.Render.Monad
import Korrvigs.Note.Render.Table
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
    writeText $ T.pack $ show i
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
writeNoteLazy doc = runRenderM 80 (render doc)

writeHeaderLazy :: Header -> BSL.ByteString
writeHeaderLazy hd = runRenderM 80 $ renderBlock $ Sub hd

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
  writeText "```" >> renderAttr attr >> flush >> newline
  renderRawText code >> flush >> newline
  writeText "```"
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
  separatedRenders seps $ for defs $ \(term, def) -> do
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
  width <- ask
  renderTable width (\w -> runRenderM w . renderTopLevel False) tbl

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
      count <- registerLink $ T.pack $ show uri
      writeText $ T.pack $ show count
      writeText "]"
renderInline Space = flush
renderInline Break = flush
renderInline (DisplayMath mth) = surrounded "$$" $ writeText mth
renderInline (InlineMath mth) = surrounded "$" $ writeText mth
renderInline (Sidenote note) = do
  num <- registerNote note
  writeText . T.pack $ "[^" <> show num <> "]"
renderInline (Check TaskTodo) = writeText "[ ]"
renderInline (Check TaskOngoing) = writeText "[-]"
renderInline (Check TaskBlocked) = writeText "[*]"
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
  writeText "title: " >> surrounded "'" (writeText title) >> flush >> newline
  unless (S.null parents) $ do
    writeText "parents: " >> flush
    withPrefix "  " $ renderToYAML True $ toJSON $ unId <$> S.toList parents
    flush >> newline
  forM_ (M.toList $ M.delete (mtdtName Title) mtdt) $ \(key, val) -> do
    writeText (CI.foldedCase key) >> writeText ": " >> flush
    withPrefix "  " $ renderToYAML True val
    flush >> newline
  writeText "..." >> flush

renderToYAML :: Bool -> Value -> RenderM ()
renderToYAML _ Null = writeText "~"
renderToYAML _ (Bool True) = writeText "true"
renderToYAML _ (Bool False) = writeText "false"
renderToYAML _ (Number num) = writeText . T.pack $ show num
renderToYAML _ (String txt) = surrounded "'" $ writeText txt
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
