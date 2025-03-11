module Korrvigs.Web.Download (getEntryDownloadR) where

import Control.Lens
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Actions
import Korrvigs.Calendar (calendarPath)
import Korrvigs.Entry
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import System.FilePath
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
downloadEntry (EventD event) =
  pure $ toTypedContent ("text/calendar" :: ContentType, ContentFile (event ^. eventFile) Nothing)
downloadEntry (CalendarD cal) = do
  path <- calendarPath cal
  pure $ toTypedContent (typeJson, ContentFile path Nothing)

suggestExtension :: KindData -> Text
suggestExtension (LinkD _) = ".json"
suggestExtension (FileD file) = T.pack $ takeExtension $ file ^. filePath
suggestExtension (NoteD _) = ".md"
suggestExtension (EventD _) = ".ics"
suggestExtension (CalendarD _) = ".json"

filenameHint :: Id -> KindData -> Handler ()
filenameHint i dat =
  addHeader "Content-Disposition" $ "inline; filename=\"" <> unId i <> suggestExtension dat <> "\""

getEntryDownloadR :: WebId -> Handler TypedContent
getEntryDownloadR (WId i) =
  load i >>= \case
    Just entry -> do
      filenameHint i $ entry ^. kindData
      downloadEntry $ entry ^. kindData
    Nothing -> notFound
