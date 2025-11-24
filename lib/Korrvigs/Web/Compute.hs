module Korrvigs.Web.Compute (getEntryComputeListR, getEntryComputeR, postEntryComputeR) where

import Conduit
import Control.Lens
import Data.Text (Text)
import Korrvigs.Compute.Run
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.SQL
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye
import Yesod

getEntryComputeListR :: WebId -> Handler Value
getEntryComputeListR (WId i) = do
  comps :: [Text] <- rSelect $ do
    cmp <- selectTable computationsTable
    nm <- nameFor $ cmp ^. sqlCompEntry
    where_ $ nm .== sqlId i
    pure $ cmp ^. sqlCompName
  pure $ toJSON comps

serveComputation ::
  (Computation -> Handler (Either Text RunnableResult)) ->
  Id ->
  Text ->
  Handler TypedContent
serveComputation runner i cmp =
  getComputation i cmp >>= \case
    Nothing -> notFound
    Just comp ->
      runner comp >>= \case
        Left err -> throwM $ KMiscError $ "Failed to run computation: " <> err
        Right res -> pure $ toTypedContent (serveType $ comp ^. cmpRun . runType, serveResult res)

getEntryComputeR :: WebId -> Text -> Handler TypedContent
getEntryComputeR (WId i) = serveComputation runVeryLazy i

postEntryComputeR :: WebId -> Text -> Handler TypedContent
postEntryComputeR (WId i) = serveComputation runForce i

serveType :: RunnableType -> ContentType
serveType ScalarImage = typeJpeg
serveType ScalarGraphic = typePng
serveType VectorGraphic = typeSvg
serveType ArbitraryJson = typeJson
serveType ArbitraryText = typePlain
serveType TabularCsv = "text/csv; charset=utf-8"

serveResult :: RunnableResult -> Content
serveResult res = toContent $ encodeToLBS res
