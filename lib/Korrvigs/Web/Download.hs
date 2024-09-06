module Korrvigs.Web.Download (getEntryDownloadR) where

import Control.Lens
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Web.Backend
import Korrvigs.Web.Login
import Korrvigs.Web.Routes
import Yesod

downloadEntry :: KindData -> Handler TypedContent
downloadEntry (LinkD link) =
  pure $ toTypedContent (typeJson, ContentFile (link ^. linkPath) Nothing)
downloadEntry (FileD file) = case file ^. fileStatus of
  FileAbsent -> notFound
  _ -> do
    pure $ toTypedContent (file ^. fileMime, ContentFile (file ^. filePath) Nothing)
downloadEntry (NoteD note) =
  pure $ toTypedContent (typePlain, ContentFile (note ^. notePath) Nothing)

getEntryDownloadR :: WebId -> Handler TypedContent
getEntryDownloadR (WId i) =
  load i >>= \case
    Just entry -> logWrap $ downloadEntry $ entry ^. kindData
    Nothing -> notFound
