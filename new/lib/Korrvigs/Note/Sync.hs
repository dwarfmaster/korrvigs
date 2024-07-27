module Korrvigs.Note.Sync where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Aeson (toJSON)
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Helpers
import Korrvigs.Note.Pandoc
import Korrvigs.Note.SQL
import Korrvigs.Utils.DateTree
import Opaleye
import System.Directory (doesFileExist, removeFile)
import System.FilePath (joinPath, takeBaseName)

dGetIdImpl :: FilePath -> Id
dGetIdImpl = MkId . T.pack . takeBaseName

noteFromRow :: NoteRow -> Entry -> Note
noteFromRow nrow entry = MkNote entry (nrow ^. sqlNotePath)

dLoadImpl :: (MonadKorrvigs m) => Id -> ((Entry -> Note) -> Entry) -> m (Maybe Entry)
dLoadImpl i cstr = do
  sel <- rSelectOne $ do
    nrow <- selectTable notesTable
    where_ $ nrow ^. sqlNoteName .== sqlId i
    pure nrow
  case (sel :: Maybe NoteRow) of
    Nothing -> pure Nothing
    Just nrow -> pure . Just . cstr $ noteFromRow nrow

dRemoveDBImpl :: (MonadKorrvigs m) => Id -> m ()
dRemoveDBImpl i =
  atomicSQL $ \conn -> do
    void $
      runDelete conn $
        Delete
          { dTable = notesTable,
            dWhere = \nrow -> nrow ^. sqlNoteName .== sqlId i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesTable,
            dWhere = \erow -> erow ^. sqlEntryName .== sqlId i,
            dReturning = rCount
          }

dRemoveImpl :: (MonadKorrvigs m) => FilePath -> m ()
dRemoveImpl path = do
  let i = dGetIdImpl path
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path
  dRemoveDBImpl i

noteDirectory :: (MonadKorrvigs m) => m FilePath
noteDirectory = joinPath . (: ["notes"]) <$> root

noteTreeType :: DateTreeType
noteTreeType = def & dtYear .~ True & dtMonth .~ True

allNotes :: (MonadKorrvigs m) => m [FilePath]
allNotes = do
  rt <- noteDirectory
  let dtt = noteTreeType
  files <- listFiles rt dtt
  pure $ (^. _1) <$> files

dListImpl :: (MonadKorrvigs m) => m (Set FilePath)
dListImpl = S.fromList <$> allNotes

dSyncImpl :: (MonadKorrvigs m) => m (Map Id RelData)
dSyncImpl =
  M.fromList <$> (allNotes >>= mapM (sequence . (dGetIdImpl &&& dSyncOneImpl)))

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m RelData
dSyncOneImpl path = do
  let i = dGetIdImpl path
  prev <- load i
  case prev of
    Nothing -> pure ()
    Just prevEntry ->
      case prevEntry ^. kindData of
        NoteD note | note ^. notePath == path -> dispatchRemoveDB prevEntry
        _ -> dispatchRemove prevEntry
  doc <- readNote path >>= throwEither (KCantLoad i)
  syncDocument i path doc
  let extras = mtdtExtras $ doc ^. docMtdt
  pure $
    RelData
      { _relSubOf = fromMaybe [] $ extras ^. mtdtParents,
        _relRefTo = S.toList $ doc ^. docRefTo
      }

syncDocument :: (MonadKorrvigs m) => Id -> FilePath -> Document -> m ()
syncDocument i path doc = do
  let mtdt = doc ^. docMtdt
  let extras = mtdtExtras mtdt
  let geom = extras ^. mtdtGeometry
  let tm = extras ^. mtdtDate
  let dur = extras ^. mtdtDuration
  let erow = EntryRow i Note tm dur geom Nothing (Just $ toJSON mtdt) :: EntryRow
  let nrow = NoteRow i path :: NoteRow
  let txt = renderDocument doc
  atomicSQL $ \conn -> do
    void $
      runInsert conn $
        Insert
          { iTable = entriesTable,
            iRows = [toFields erow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    void $
      runInsert conn $
        Insert
          { iTable = notesTable,
            iRows = [toFields nrow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    void $
      runUpdate conn $
        Update
          { uTable = entriesTable,
            uUpdateWith = sqlEntryText .~ toNullable (tsParseEnglish $ sqlStrictText txt),
            uWhere = \row -> row ^. sqlEntryName .== sqlId i,
            uReturning = rCount
          }
  pure ()
