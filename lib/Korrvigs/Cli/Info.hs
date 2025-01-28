module Korrvigs.Cli.Info where

import Conduit (throwM)
import Control.Lens hiding (argument)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy.UTF8 as BSL8
import Data.Char (toUpper)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import qualified Data.Text.IO as TIO
import Data.Text.Manipulate (mapHead)
import Data.Time.Format.ISO8601 (iso8601Show)
import qualified Data.Vector as V
import Korrvigs.Actions.Load (loadMetadata)
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils.Lens
import Options.Applicative hiding (value)
import Text.Builder (decimal, string, text)
import qualified Text.Builder as Bld
import Prelude

data Cmd = Cmd
  { _json :: Bool,
    _target :: Id
  }

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  Cmd
    <$> switch (long "json" <> help "Display info as json")
    <*> (MkId <$> argument str (metavar "ID"))

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Display information about a Korrvigs entry"
      <> header "korr info -- Show information about entry"

run :: Cmd -> KorrM ()
run (Cmd js i) =
  load i >>= \case
    Nothing -> throwM $ KIdNotFound i
    Just entry -> do
      mtdt <- loadMetadata i
      liftIO $ if js then displayEntryJSON entry mtdt else displayEntry entry mtdt

entryInfoSpec :: (Contravariant f, Applicative f) => [(Text, (Bld.Builder -> f Bld.Builder) -> (Entry, Metadata) -> f (Entry, Metadata))]
entryInfoSpec =
  [ -- Generic info
    ("name", _1 . name . to unId . to text),
    ("kind", _1 . kind . to displayKind . to text),
    ("title", _2 . ix (mtdtName Title) . _String . to text),
    ("date", _1 . date . _Just . to iso8601Show . to string),
    ("duration", _1 . duration . _Just . to iso8601Show . to string),
    ("pages", _2 . ix (mtdtName Pages) . _Integer . to decimal),
    ("width", _2 . ix (mtdtName Width) . _Integer . to decimal),
    ("height", _2 . ix (mtdtName Height) . _Integer . to decimal),
    -- Note info
    ("path", _1 . _Note . notePath . to string),
    -- Link info
    ("protocol", _1 . _Link . linkProtocol . to text),
    ("ref", _1 . _Link . linkRef . to text),
    ("path", _1 . _Link . linkPath . to string),
    -- File info
    ("path", _1 . _File . filePath . to string),
    ("status", _1 . _File . fileStatus . to displayFileStatus . to text),
    ("mime", _1 . _File . fileMime . to Enc.decodeUtf8 . to text)
  ]

buildInfoLine :: Entry -> Metadata -> (Text, (Bld.Builder -> Const [Bld.Builder] Bld.Builder) -> (Entry, Metadata) -> Const [Bld.Builder] (Entry, Metadata)) -> Bool -> Bld.Builder
buildInfoLine entry mtdt (nm', getter) first = case lst of
  [] -> mempty
  _ -> nm <> mconcat (intersperse " " lst)
  where
    nm = (if first then mempty else "\n") <> text (mapHead toUpper nm') <> ": "
    lst = toMonoid (: []) getter (entry, mtdt)

displayEntry :: Entry -> Metadata -> IO ()
displayEntry entry mtdt =
  TIO.putStrLn $
    Bld.run $
      mconcat $
        fmap (uncurry $ buildInfoLine entry mtdt) $
          zip entryInfoSpec $
            True : repeat False

buildInfoJSON :: Entry -> Metadata -> (Text, (Bld.Builder -> Const [Value] Bld.Builder) -> (Entry, Metadata) -> Const [Value] (Entry, Metadata)) -> Map Text Value
buildInfoJSON entry mtdt (nm, getter) = case lst of
  [] -> M.empty
  [v] -> M.singleton nm v
  _ -> M.singleton nm $ Array $ V.fromList lst
  where
    lst = toMonoid ((: []) . String . Bld.run) getter (entry, mtdt)

displayEntryJSON :: Entry -> Metadata -> IO ()
displayEntryJSON entry mtdt = putStrLn $ BSL8.toString obj
  where
    obj = encodingToLazyByteString $ value $ toJSON mp
    mp = mconcat $ fmap (buildInfoJSON entry mtdt) entryInfoSpec
