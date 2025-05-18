module Korrvigs.Note.Render.Monad where

import Control.Lens
import Control.Monad
import Control.Monad.RWS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Note.AST
import Prelude hiding (break)

data RenderState = RS
  { _break :: Int,
    _cursor :: Int,
    _prefixes :: [Text],
    _prefixLen :: Int,
    _symbol :: Maybe (Builder, Int),
    _notes :: [(Int, [Block])],
    _noteCount :: Int,
    _linkCount :: Int,
    _links :: [(Int, Text)], -- Links to add at the end of section
    _listDepth :: Int
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
flush =
  use symbol >>= \case
    Nothing -> pure ()
    Just (sym, l) -> do
      symbol .= Nothing
      pos <- use cursor
      mx <- ask
      br <- use doBreak
      st <- use atStart
      unless st $ if br && pos + l > mx then newline else space
      tell sym
      cursor += l

-- Insert a newline, with the prefix
newline :: RenderM ()
newline = do
  prefs <- use prefixes
  let prefix = mconcat . reverse $ B.stringUtf8 . T.unpack <$> prefs
  prefixL <- use prefixLen
  tell $ B.charUtf8 '\n' <> prefix
  cursor .= prefixL
  pure ()

-- Insert a space
space :: RenderM ()
space = do
  tell $ B.charUtf8 ' '
  cursor += 1

-- Add text to symbol
writeText :: Text -> RenderM ()
writeText t = symbol %= symAppend (B.stringUtf8 $ T.unpack t) (T.length t)

symAppend :: Builder -> Int -> Maybe (Builder, Int) -> Maybe (Builder, Int)
symAppend bd l Nothing = Just (bd, l)
symAppend bd l (Just (b, l')) = Just (b <> bd, l + l')

-- Add utf8-encoded bytestring to symbol
writeBS :: BSL.ByteString -> RenderM ()
writeBS bs = symbol %= symAppend (B.lazyByteString bs) (BSL8.length bs)

-- Render some text with an addition prefix (will only take effect after the
-- next newline).
withPrefix :: Text -> RenderM a -> RenderM a
withPrefix prefix act = do
  prefixes %= (prefix :)
  prefixLen += T.length prefix
  r <- act
  prefixes %= drop 1
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

registerLink :: Text -> RenderM Int
registerLink uri = do
  num <- use linkCount
  links %= ((num, uri) :)
  linkCount += 1
  pure num

hasNotes :: RenderM Bool
hasNotes = use $ notes . to (not . null)

hasLinks :: RenderM Bool
hasLinks = use $ links . to (not . null)

separatedRenders :: Int -> [RenderM ()] -> RenderM ()
separatedRenders n rdrs =
  forM_ (zip [1 ..] rdrs) $ \(i, rdr) -> do
    rdr >> flush
    unless (i == length rdrs) $ replicateM_ n newline

for :: (Traversable t) => t a -> (a -> b) -> t b
for = flip fmap

runRenderM :: Int -> RenderM () -> BSL.ByteString
runRenderM width rdr =
  B.toLazyByteString . (^. _3) $
    runRWS rdr width $
      RS
        { _break = 0,
          _cursor = 0,
          _prefixes = [],
          _prefixLen = 0,
          _symbol = Nothing,
          _notes = [],
          _noteCount = 1,
          _linkCount = 0,
          _links = [],
          _listDepth = 0
        }
