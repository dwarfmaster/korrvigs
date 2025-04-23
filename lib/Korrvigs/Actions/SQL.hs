module Korrvigs.Actions.SQL
  ( load,
    loadMetadata,
    removeKindDB,
    removeDB,
    SyncData (..),
    syncSQL,
    syncRelsSQL,
    syncEntryRow,
    syncDataRow,
    syncMtdtRows,
    syncTextContent,
    syncParents,
    syncRefs,
  )
where

import Control.Lens
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Profunctor.Product.Default
import Data.Text (Text)
import GHC.Int (Int64)
import qualified Korrvigs.Calendar.SQL as Cal
import Korrvigs.Compute.Action
import Korrvigs.Entry
import qualified Korrvigs.Event.SQL as Event
import Korrvigs.FTS
import qualified Korrvigs.File.SQL as File
import Korrvigs.Kind
import qualified Korrvigs.Link.SQL as Link
import Korrvigs.Monad
import qualified Korrvigs.Note.SQL as Note
import Opaleye hiding (null)

mkEntry :: (IsKindData a) => EntryRow -> (Entry -> a) -> Entry
mkEntry row = kdEntry . entryFromRow kdKindData row

load :: (MonadKorrvigs m) => Id -> m (Maybe Entry)
load i = do
  mrow <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  case mrow of
    Nothing -> pure Nothing
    Just row -> do
      case row ^. sqlEntryKind of
        Note -> Note.sqlLoad i $ mkEntry row
        Link -> Link.sqlLoad i $ mkEntry row
        File -> File.sqlLoad i $ mkEntry row
        Event -> Event.sqlLoad i $ mkEntry row
        Calendar -> Cal.sqlLoad i $ mkEntry row

loadMetadata :: (MonadKorrvigs m) => Id -> m Metadata
loadMetadata i = do
  mtdt <- rSelect $ do
    mtdtRow <- selectTable entriesMetadataTable
    where_ $ mtdtRow ^. sqlEntry .== sqlId i
    pure (mtdtRow ^. sqlKey, mtdtRow ^. sqlValue)
  pure $ M.fromList mtdt

dispatchRemove :: (MonadKorrvigs m) => (Id -> [Delete Int64] -> m ()) -> KindData -> m ()
dispatchRemove rm (LinkD lnk) =
  let i = lnk ^. linkEntry . name in rm i $ Link.sqlRemove i
dispatchRemove rm (NoteD note) =
  let i = note ^. noteEntry . name in rm i $ Note.sqlRemove i
dispatchRemove rm (FileD file) =
  let i = file ^. fileEntry . name in rm i $ File.sqlRemove i
dispatchRemove rm (EventD ev) =
  let i = ev ^. eventEntry . name in rm i $ Event.sqlRemove i
dispatchRemove rm (CalendarD cal) =
  let i = cal ^. calEntry . name in rm i $ Cal.sqlRemove i

removeKindDB :: (MonadKorrvigs m) => Entry -> m ()
removeKindDB entry = dispatchRemove (\_ dels -> atomicSQL $ \conn -> forM_ dels $ runDelete conn) $ entry ^. kindData

genRemoveDB :: (MonadKorrvigs m) => Id -> [Delete Int64] -> m ()
genRemoveDB i dels =
  atomicSQL $ \conn -> do
    forM_ dels $ runDelete conn
    void $
      runDelete conn $
        Delete
          { dTable = entriesMetadataTable,
            dWhere = \mrow -> mrow ^. sqlEntry .== sqlId i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesSubTable,
            dWhere = \row -> row ^. source .== sqlId i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesRefTable,
            dWhere = \row -> row ^. source .== sqlId i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesTable,
            dWhere = \erow -> erow ^. sqlEntryName .== sqlId i,
            dReturning = rCount
          }

removeDB :: (MonadKorrvigs m) => Entry -> m ()
removeDB entry = dispatchRemove genRemoveDB $ entry ^. kindData

data SyncData drow = SyncData
  { _syncEntryRow :: EntryRow,
    _syncDataRow :: drow,
    _syncMtdtRows :: [MetadataRow],
    _syncTextContent :: Maybe Text,
    _syncParents :: [Id],
    _syncRefs :: [Id],
    _syncCompute :: Map Text Action
  }

makeLenses ''SyncData

syncSQL :: (MonadKorrvigs m, Default ToFields hs sql) => Table sql sql -> SyncData hs -> m ()
syncSQL tbl dt = atomicSQL $ \conn -> do
  let i = dt ^. syncEntryRow . sqlEntryName
  -- Update entry
  mprev :: [EntryRow] <- runSelect conn $ limit 1 $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  case listToMaybe mprev of
    Nothing ->
      void $
        runInsert conn $
          Insert
            { iTable = entriesTable,
              iRows = [toFields $ dt ^. syncEntryRow],
              iReturning = rCount,
              iOnConflict = Just doNothing
            }
    Just prev -> do
      case prev ^. sqlEntryKind of
        Link -> mapM_ (runDelete conn) $ Link.sqlRemove i
        Note -> mapM_ (runDelete conn) $ Note.sqlRemove i
        File -> mapM_ (runDelete conn) $ File.sqlRemove i
        Event -> mapM_ (runDelete conn) $ Event.sqlRemove i
        Calendar -> mapM_ (runDelete conn) $ Cal.sqlRemove i
      void $
        runUpdate conn $
          Update
            { uTable = entriesTable,
              uUpdateWith = const $ toFields $ dt ^. syncEntryRow,
              uWhere = \row -> row ^. sqlEntryName .== sqlId i,
              uReturning = rCount
            }
  void $
    runInsert conn $
      Insert
        { iTable = tbl,
          iRows = [toFields $ dt ^. syncDataRow],
          iReturning = rCount,
          iOnConflict = Just doNothing
        }
  -- Update metadata
  void $
    runDelete conn $
      Delete
        { dTable = entriesMetadataTable,
          dWhere = \mtdt -> mtdt ^. sqlEntry .== sqlId i,
          dReturning = rCount
        }
  void $
    runInsert conn $
      Insert
        { iTable = entriesMetadataTable,
          iRows = toFields <$> dt ^. syncMtdtRows,
          iReturning = rCount,
          iOnConflict = Just doNothing
        }
  -- Optionally set textContent
  forM_ (dt ^. syncTextContent) $ \txt ->
    runUpdate conn $
      Update
        { uTable = entriesTable,
          uUpdateWith = sqlEntryText .~ toNullable (tsParseEnglish $ sqlStrictText txt),
          uWhere = \row -> row ^. sqlEntryName .== sqlId i,
          uReturning = rCount
        }
  -- Update computations
  let compRows = uncurry (CompRow i) <$> M.toList (dt ^. syncCompute)
  void $
    runDelete conn $
      Delete
        { dTable = computationsTable,
          dWhere = \comp -> comp ^. sqlCompEntry .== sqlId i,
          dReturning = rCount
        }
  void $
    runInsert conn $
      Insert
        { iTable = computationsTable,
          iRows = toFields <$> compRows,
          iReturning = rCount,
          iOnConflict = Just doNothing
        }

syncRelsSQL :: (MonadKorrvigs m) => Id -> [Id] -> [Id] -> m ()
syncRelsSQL i subsOf relsTo = atomicSQL $ \conn -> do
  -- Sync parents
  void $
    runDelete conn $
      Delete
        { dTable = entriesSubTable,
          dWhere = \sb -> sb ^. source .== sqlId i,
          dReturning = rCount
        }
  unless (null subsOf) $
    void $
      runInsert conn $
        insertSubOf $
          (i,) <$> subsOf
  -- Sync references
  void $
    runDelete conn $
      Delete
        { dTable = entriesRefTable,
          dWhere = \sb -> sb ^. source .== sqlId i,
          dReturning = rCount
        }
  unless (null relsTo) $
    void $
      runInsert conn $
        insertRefTo $
          (i,) <$> relsTo
