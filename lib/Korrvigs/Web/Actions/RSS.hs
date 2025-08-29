module Korrvigs.Web.Actions.RSS where

import Control.Lens
import Data.Aeson.Lens
import Data.Default
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import Korrvigs.Entry.New
import Korrvigs.Link.Download
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad.Metadata
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

importRssTarget :: ActionTarget -> Bool
importRssTarget (TargetEntry _) = True
importRssTarget _ = False

importRssForm :: AForm Handler Text
importRssForm = areq textField "URL" Nothing

importRssTitle :: ActionTarget -> Text
importRssTitle = const "Import RSS from"

runImportRSS :: Text -> ActionTarget -> Handler ActionReaction
runImportRSS url (TargetEntry entry) = do
  info <- flip appEndo def <$> downloadInformation url
  render <- getUrlRenderParams
  case info ^? neMtdt . at (mtdtName Feed) . _Just . _String of
    Nothing -> pure $ def & reactMsg ?~ [hamlet|<p>No feed found for <a href=#{url}>#{url}</a>|] render
    Just feed -> do
      updateMetadata entry (M.singleton (mtdtSqlName Feed) (toJSON feed)) []
      pure $ def & reactMsg ?~ [hamlet|<p>Found feed <a href=#{feed}>#{feed}</a>|] render
runImportRSS _ _ = pure def
