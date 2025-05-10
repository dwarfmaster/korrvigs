module Korrvigs.Web.Actions.Update where

import Control.Lens
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import qualified Korrvigs.File.New as New
import Korrvigs.Kind
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import System.FilePath
import System.IO.Temp
import Yesod hiding (joinPath)

updateTarget :: ActionTarget -> Bool
updateTarget (TargetEntry entry) | entry ^. kind == File = True
updateTarget _ = False

updateForm :: AForm Handler FileInfo
updateForm = fileAFormReq ("" {fsLabel = "File"})

updateTitle :: ActionTarget -> Text
updateTitle = const "Update"

runUpdate :: FileInfo -> ActionTarget -> Handler ActionReaction
runUpdate content (TargetEntry entry) = case entry ^. kindData of
  FileD file -> withRunInIO $ \runIO ->
    withSystemTempDirectory "korrUpload" $ \dir -> do
      let filename = T.unpack $ fileName content
      let path = joinPath [dir, filename]
      fileMove content path
      runIO $ New.update file path
      render <- runIO getUrlRender
      pure $ def & reactRedirect ?~ render (EntryR $ WId $ entry ^. name)
  _ -> pure def
runUpdate _ _ = pure def
