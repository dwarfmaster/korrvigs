module Korrvigs.Web.Compute (getEntryCacheR, getEntryComputeR) where

import Control.Lens
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Compute
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import System.Directory
import Yesod

getEntryCacheR :: WebId -> Handler Value
getEntryCacheR (WId i) = do
  comps <- entryStoredComputations i
  pure $ toJSON $ fst <$> M.toList comps

getEntryComputeR :: WebId -> Text -> Handler TypedContent
getEntryComputeR (WId i) cmpName = do
  comps <- entryStoredComputations i
  cmp <- maybe notFound pure $ M.lookup cmpName comps
  path <- compFile cmp
  ex <- liftIO $ doesFileExist path
  if ex
    then serveCached path $ cmp ^. cmpType
    else notFound

serveCached :: FilePath -> CompType -> Handler TypedContent
serveCached path ScalarImage = pure $ toTypedContent (typeJpeg, ContentFile path Nothing)
serveCached path Picture = pure $ toTypedContent (typePng, ContentFile path Nothing)
serveCached path VectorImage = pure $ toTypedContent (typeSvg, ContentFile path Nothing)
