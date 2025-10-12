module Korrvigs.Web.Download (getEntryDownloadR) where

import Control.Lens
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Calendar (calendarPath)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry
import Korrvigs.Web.Routes
import System.FilePath
import Yesod

downloadEntry :: KindData -> Handler TypedContent
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
downloadEntry (SyndicateD syn) =
  pure $ toTypedContent (typeJson, ContentFile (syn ^. synPath) Nothing)

suggestExtension :: KindData -> Text
suggestExtension (FileD file) = T.pack $ takeExtension $ file ^. filePath
suggestExtension (NoteD _) = ".md"
suggestExtension (EventD _) = ".ics"
suggestExtension (CalendarD _) = ".json"
suggestExtension (SyndicateD _) = ".json"

filenameHint :: Id -> KindData -> Handler ()
filenameHint i dat =
  addHeader "Content-Disposition" $ "inline; filename=\"" <> unId i <> suggestExtension dat <> "\""

getEntryDownloadR :: WebId -> Handler TypedContent
getEntryDownloadR (WId i) =
  load i >>= \case
    Just entry -> do
      public <- isPublic
      private <- isPrivate entry
      when (public && private) $ permissionDenied "Tried to access a private entry"
      filenameHint i $ entry ^. entryKindData
      downloadEntry $ entry ^. entryKindData
    Nothing -> notFound
