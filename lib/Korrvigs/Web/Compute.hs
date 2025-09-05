module Korrvigs.Web.Compute (getEntryCacheR, getEntryComputeR) where

import Control.Lens
import Data.Text (Text)
import Korrvigs.Compute
import Korrvigs.Compute.Action
import Korrvigs.Compute.Declare
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye
import Yesod

getEntryCacheR :: WebId -> Handler Value
getEntryCacheR (WId i) = do
  comps :: [Text] <- rSelect $ do
    cmp <- selectTable computationsTable
    nm <- nameFor $ cmp ^. sqlCompEntry
    where_ $ nm .== sqlId i
    pure $ cmp ^. sqlCompName
  pure $ toJSON comps

getEntryComputeR :: WebId -> Text -> Handler TypedContent
getEntryComputeR (WId i) cmpName = do
  mcmp <- rSelectOne $ do
    sqlI <- fromName pure $ sqlId i
    view sqlCompAction <$> selComp sqlI cmpName
  cmp <- maybe notFound pure mcmp
  path <- lazyRun i cmpName cmp
  let cmpType = actionData cmp ^. adatType
  pure $ toTypedContent (serveType cmpType, ContentFile path Nothing)

serveType :: CompType -> ContentType
serveType ScalarImage = typeJpeg
serveType Picture = typePng
serveType VectorImage = typeSvg
serveType Json = typeJson
