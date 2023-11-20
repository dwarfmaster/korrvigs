module Korrvigs.Web.Sub (widget) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (readFile)
import Korrvigs.Definition
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.UUID as U
import Network.Mime
import System.FilePath.Posix (takeExtension)
import Yesod
import Prelude hiding (readFile)

-- Receives the path of the file, the mime type and the extension (with the dot)
widgetForSub :: FilePath -> MimeType -> Text -> Handler (Maybe Widget)
widgetForSub path mime _ | BS.isPrefixOf "text/" mime = Just <$> widgetForText path
widgetForSub path _ ".dhall" = Just <$> widgetForText path
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
  let mime = defaultMimeLookup sub
  wdgt <- widgetForSub path mime $ T.pack $ takeExtension $ T.unpack sub
  pure
    [whamlet|
    <h1>
      <a href=@{EntryR (U.UUID (entry_id entry))}>
        #{entry_name entry}
      <span .sub>
        #{sub}
    $maybe w <- wdgt
      ^{w}
  |]
