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
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Utils.Lens
import Options.Applicative hiding (value)
import Text.Builder (string, text)
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
    Just entry ->
      liftIO $ if js then displayEntryJSON entry else displayEntry entry

entryInfoSpec :: (Contravariant f, Applicative f) => [(Text, (Bld.Builder -> f Bld.Builder) -> Entry -> f Entry)]
entryInfoSpec =
  [ -- Generic info
    ("name", name . to unId . to text),
    ("kind", kind . to displayKind . to text),
    ("title", metadata . ix "title" . metaValue . _String . to text),
    ("date", date . _Just . to iso8601Show . to string),
    ("duration", duration . _Just . to iso8601Show . to string),
    -- Note info
    ("path", _Note . notePath . to string),
    -- Link info
    ("protocol", _Link . linkProtocol . to text),
    ("ref", _Link . linkRef . to text),
    ("path", _Link . linkPath . to string),
    -- File info
    ("path", _File . filePath . to string),
    ("status", _File . fileStatus . to displayFileStatus . to text),
    ("mime", _File . fileMime . to Enc.decodeUtf8 . to text)
  ]

buildInfoLine :: Entry -> (Text, (Bld.Builder -> Const [Bld.Builder] Bld.Builder) -> Entry -> Const [Bld.Builder] Entry) -> Bool -> Bld.Builder
buildInfoLine entry (nm', getter) first = case lst of
  [] -> mempty
  _ -> nm <> mconcat (intersperse " " lst)
  where
    nm = (if first then mempty else "\n") <> text (mapHead toUpper nm') <> ": "
    lst = toMonoid (: []) getter entry

displayEntry :: Entry -> IO ()
displayEntry entry =
  TIO.putStrLn $
    Bld.run $
      mconcat $
        fmap (uncurry $ buildInfoLine entry) $
          zip entryInfoSpec $
            True : repeat False

buildInfoJSON :: Entry -> (Text, (Bld.Builder -> Const [Value] Bld.Builder) -> Entry -> Const [Value] Entry) -> Map Text Value
buildInfoJSON entry (nm, getter) = case lst of
  [] -> M.empty
  [v] -> M.singleton nm v
  _ -> M.singleton nm $ Array $ V.fromList lst
  where
    lst = toMonoid ((: []) . String . Bld.run) getter entry

displayEntryJSON :: Entry -> IO ()
displayEntryJSON entry = putStrLn $ BSL8.toString obj
  where
    obj = encodingToLazyByteString $ value $ toJSON mp
    mp = mconcat $ fmap (buildInfoJSON entry) entryInfoSpec
