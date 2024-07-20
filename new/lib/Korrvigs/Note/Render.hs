module Korrvigs.Note.Render (writeNote) where

import Control.Exception (SomeException, try)
import Control.Lens
import Control.Monad.Extra (whenM)
import Control.Monad.RWS
import Data.Aeson (Value (..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (hPutStr)
import qualified Data.Vector as V
import Korrvigs.Entry.Ident
import Korrvigs.Note.AST
import System.IO hiding (hPutStr)
import Text.Builder (Builder)
import qualified Text.Builder as B
import Prelude hiding (break)

data RenderState = RS
  { _break :: Int,
    _cursor :: Int,
    _prefixes :: [Text],
    _prefixLen :: Int,
    _symbol :: Builder,
    _notes :: [(Int, [Block])],
    _noteCount :: Int
  }

makeLenses ''RenderState

doBreak :: Getter RenderState Bool
doBreak = break . to (== 0)

atStart :: Getter RenderState Bool
atStart = to (\x -> (x, x)) . alongside cursor prefixLen . to (uncurry (==))

-- The read part is the allowed width of the rendered document.
-- The write part is the document itself.
-- The state is the current cursor position in terms of width with the symbol,
-- and a stack of prefixes.
type RenderM = RWS Int Builder RenderState

-- Clear the currently building symbol and adds it to the document.
flush :: RenderM ()
flush = do
  sym <- use symbol
  if B.null sym
    then pure ()
    else do
      symbol .= mempty
      pos <- use cursor
      let l = B.length sym
      mx <- ask
      br <- use doBreak
      st <- use atStart
      unless st $ if br && pos + l > mx then newline else space
      tell sym
      cursor += B.length sym

-- Insert a newline, with the prefix
newline :: RenderM ()
newline = do
  prefs <- use prefixes
  let prefix = mconcat . reverse $ B.text <$> prefs
  prefixL <- use prefixLen
  tell $ B.char '\n' <> prefix
  cursor .= prefixL
  pure ()

-- Insert a space
space :: RenderM ()
space = do
  tell $ B.char ' '
  cursor += 1

-- Add text to symbol
writeText :: Text -> RenderM ()
writeText t = symbol %= (<> B.text t)

-- Render some text with an addition prefix (will only take effect after the
-- next newline).
withPrefix :: Text -> RenderM a -> RenderM a
withPrefix prefix act = do
  prefixes %= (prefix :)
  prefixLen += T.length prefix
  r <- act
  prefixes %= tail
  prefixLen -= T.length prefix
  pure r

-- Execute action with prefix, also including it at the current position
doPrefix :: Text -> RenderM a -> RenderM a
doPrefix prefix act = writeText prefix >> withoutBreak flush >> withPrefix prefix act

-- Render a list with begin delimiter, sperator in between element and end
-- delimiter. If the list is empty nothing is rendered.
listOnLine :: [RenderM a] -> RenderM b -> RenderM c -> RenderM d -> RenderM ()
listOnLine [] _ _ _ = pure ()
listOnLine (i : is) beg sep end = do
  void beg
  void i
  forM_ is $ \i' -> void sep >> i'
  void end

withoutBreak :: RenderM a -> RenderM a
withoutBreak act = do
  break += 1
  r <- act
  break -= 1
  pure r

registerNote :: [Block] -> RenderM Int
registerNote note = do
  num <- use noteCount
  notes %= ((num, note) :)
  noteCount += 1
  pure num

flushNotes :: RenderM ()
flushNotes = do
  nts <- use notes
  separatedRenders 2 $ for (reverse nts) $ \(i, note) -> do
    writeText . T.pack $ "[^" <> show i <> "]:"
    withoutBreak flush
    withPrefix "  " $ separatedBks 2 note
  notes .= []

hasNotes :: RenderM Bool
hasNotes = use $ notes . to (not . null)

writeNote :: (MonadIO m) => Handle -> Document -> m (Maybe Text)
writeNote file doc = do
  let builder =
        runRWS (render doc) 80 $
          RS
            { _break = 0,
              _cursor = 0,
              _prefixes = [],
              _prefixLen = 0,
              _symbol = mempty,
              _notes = [],
              _noteCount = 1
            }
  liftIO $ do
    r <- try $ hPutStr file $ B.run $ builder ^. _3 :: IO (Either SomeException ())
    case r of
      Left e -> pure . Just . T.pack $ "IO Error: " <> show e
      Right () -> pure Nothing

render :: Document -> RenderM ()
render doc = do
  renderMetadata (doc ^. docTitle) $ doc ^. docMtdt
  withoutBreak flush
  replicateM_ 2 newline
  renderTopLevel $ doc ^. docContent
  replicateM_ 2 newline

renderTopLevel :: [Block] -> RenderM ()
renderTopLevel bks =
  separatedRenders 2 $ for bks $ \bk -> do
    renderBlock bk >> flush
    whenM hasNotes $ replicateM_ 2 newline >> flushNotes >> flush

renderRawText :: Text -> RenderM ()
renderRawText txt =
  let lns = T.lines txt
   in case lns of
        [] -> pure ()
        l1 : ls -> do
          writeText l1
          forM_ ls $ \l -> flush >> newline >> writeText l

separatedRenders :: Int -> [RenderM ()] -> RenderM ()
separatedRenders n rdrs =
  forM_ (zip [1 ..] rdrs) $ \(i, rdr) -> do
    rdr >> flush
    unless (i == length rdrs) $ replicateM_ n newline

separatedBks :: Int -> [Block] -> RenderM ()
separatedBks n = separatedRenders n . map renderBlock

for :: (Traversable t) => t a -> (a -> b) -> t b
for = flip fmap

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
    unless (i == length bks) newline
  where
    shift :: Int
    shift = (3 +) . floor . logBase (10.0 :: Double) . fromIntegral $ length bks
    prefix :: Text
    prefix = mconcat $ replicate shift " "
    renderNum :: Int -> Text
    renderNum i =
      let num = T.pack $ show i
       in num <> "." <> mconcat (replicate (shift - T.length num - 1) " ")
renderBlock (BulletList bks) =
  separatedRenders 1 $ for bks $ \bk -> do
    writeText "- " >> withoutBreak flush
    withPrefix "  " $ separatedBks 2 bk
renderBlock (DefinitionList defs) =
  separatedRenders 1 $ for defs $ \(term, def) -> do
    withoutBreak $ do
      forM_ term renderInline
      flush
    newline
    separatedRenders 1 $ for def $ \bk -> do
      writeText ": " >> withoutBreak flush
      withPrefix "  " $ separatedBks 2 bk
      flush
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
renderBlock (Sub header) = do
  writeText $ mconcat $ replicate (header ^. hdLevel) "#"
  writeText " "
  writeText $ header ^. hdTitle
  writeText " "
  renderAttr $ header ^. hdAttr
  flush
  unless (null $ header ^. hdContent) $ replicateM_ 2 newline
  renderTopLevel $ header ^. hdContent
renderBlock (Table _) = writeText "<<TODO: table>>"

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
renderInline (PlainLink uri) =
  writeText "<" >> writeText (T.pack $ show uri) >> writeText ">"
renderInline Space = flush
renderInline Break = flush
renderInline (DisplayMath mth) = surrounded "$$" $ writeText mth
renderInline (InlineMath mth) = surrounded "$" $ writeText mth
renderInline (Sidenote note) = do
  num <- registerNote note
  writeText . T.pack $ "[^" <> show num <> "]"

renderAttr :: Attr -> RenderM ()
renderAttr attr = listOnLine as (writeText "{") (writeText " ") (writeText "}")
  where
    i = [writeText "#" >> writeText (attr ^. attrId) | not . T.null $ attr ^. attrId]
    cls = [writeText "." >> writeText c | c <- attr ^. attrClasses]
    attributes = [writeText key >> writeText "=\"" >> writeText value >> writeText "\"" | (key, value) <- M.toList (attr ^. attrMtdt)]
    as = i ++ cls ++ attributes

renderMetadata :: Text -> Map Text Value -> RenderM ()
renderMetadata title mtdt = withoutBreak $ do
  writeText "---" >> flush >> newline
  writeText "title: " >> writeText title >> flush >> newline
  forM_ (M.toList $ M.delete "title" mtdt) $ \(key, val) -> do
    writeText key >> writeText ": " >> flush
    withPrefix "  " $ renderToYAML True val
    flush >> newline
  writeText "..." >> flush

renderToYAML :: Bool -> Value -> RenderM ()
renderToYAML _ Null = writeText "~"
renderToYAML _ (Bool True) = writeText "true"
renderToYAML _ (Bool False) = writeText "false"
renderToYAML _ (Number num) = writeText . T.pack $ show num
renderToYAML _ (String txt) = writeText txt
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
