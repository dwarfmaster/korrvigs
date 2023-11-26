module Korrvigs.Web.Sub (widget, mimeFor) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Korrvigs.Definition
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Query (listQueries)
import qualified Korrvigs.Web.Entry.Query as Query
import qualified Korrvigs.Web.UUID as U
import Network.Mime
import System.FilePath.Posix (takeExtension)
import Yesod
import Prelude hiding (readFile)

mimeFor :: Text -> MimeType
mimeFor = mimeByExt mimeMap defaultMimeType
  where
    mimeMap :: MimeMap
    mimeMap =
      foldr
        (uncurry M.insert)
        defaultMimeMap
        [("dhall", "text/plain")]

-- Receives the path of the file, the mime type and the extension (with the dot)
widgetForSub :: FilePath -> MimeType -> Text -> Handler (Maybe Widget)
widgetForSub path mime _ | BS.isPrefixOf "text/" mime = Just <$> widgetForText path
widgetForSub _ _ _ = pure Nothing

widgetForText :: FilePath -> Handler Widget
widgetForText path = do
  content <- liftIO $ readFile path
  pure
    [whamlet|
    <code>
      #{content}
  |]

widget :: Entry -> Text -> Handler Widget
widget entry sub = do
  root <- korrRoot
  let path = entrySubPath root entry sub
  let mime = mimeFor sub
  wdgt <- widgetForSub path mime $ T.pack $ takeExtension $ T.unpack sub
  let ent = (entry_root entry) {entity_sub = Just sub}
  queries <- listQueries ent
  pure
    [whamlet|
    <h1>
      <a href=@{EntryR (U.UUID (entry_id entry))}>
        #{entry_name entry}
      <span .sub>
        #{sub}
    <h2>
      Content
    <p>
      <a href=@{EntrySubContentR (U.UUID (entry_id entry)) sub}>
        Download
    $maybe w <- wdgt
      ^{w}
    $if null queries
      <h2>
        Queries
      ^{Query.mkList ent queries}
  |]
