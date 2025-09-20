module Korrvigs.Monad.SQL
  ( load,
    loadSql,
    loadMetadata,
    removeKindDB,
    removeDB,
    SyncData (..),
    syncSQL,
    syncRelsSQL,
    syncEntryRow,
    syncDataRows,
    syncMtdtRows,
    syncTextContent,
    syncTitle,
    syncParents,
    syncRefs,
    indexedMetadata,
    updateInMetadata,
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Int (Int64)
import qualified Korrvigs.Calendar.SQL as Cal
import Korrvigs.Compute.Action
import Korrvigs.Entry
import qualified Korrvigs.Event.SQL as Event
import Korrvigs.FTS
import qualified Korrvigs.File.SQL as File
import Korrvigs.Kind
import qualified Korrvigs.Link.SQL as Link
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad.Class
import qualified Korrvigs.Note.SQL as Note
import qualified Korrvigs.Syndicate.SQL as Syn
import Opaleye hiding (null)

indexedMetadata :: Set (CI Text)
indexedMetadata =
  S.fromList
    [ mtdtName Abstract
    ]

idMetadata :: Set (CI Text)
idMetadata =
  S.fromList
    [ mtdtName Cover
    ]

class FromCI a where
  fromCI :: CI Text -> a

instance FromCI Text where
  fromCI = CI.foldedCase

instance FromCI (CI Text) where
  fromCI = id

updateInMetadata :: (FromCI a, Ord a) => Id -> Maybe Id -> Map a Value -> Map a Value
updateInMetadata old new mtdt = foldr updateOne mtdt $ S.toList idMetadata
  where
    updateOne nm' m = case M.lookup nm m of
      Just (String v) | v == unId old -> case new of
        Nothing -> M.delete nm m
        Just nv -> M.insert nm (toJSON $ unId nv) m
      _ -> m
      where
        nm = fromCI nm'

mkEntry :: (IsKindData a) => EntryRowR -> (Entry -> a) -> Entry
mkEntry row = kdEntry . entryFromRow kdKindData row

loadImpl :: (MonadKorrvigs m) => Maybe EntryRowR -> m (Maybe Entry)
loadImpl Nothing = pure Nothing
loadImpl (Just row) =
  let sqlI = row ^. sqlEntryId
   in case row ^. sqlEntryKind of
        Note -> Note.sqlLoad sqlI $ mkEntry row
        Link -> Link.sqlLoad sqlI $ mkEntry row
        File -> File.sqlLoad sqlI $ mkEntry row
        Event -> Event.sqlLoad sqlI $ mkEntry row
        Calendar -> Cal.sqlLoad sqlI $ mkEntry row
        Syndicate -> Syn.sqlLoad sqlI $ mkEntry row

loadSql :: (MonadKorrvigs m) => Int -> m (Maybe Entry)
loadSql sqlI = do
  mrow <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== sqlInt4 sqlI
    pure entry
  loadImpl mrow

load :: (MonadKorrvigs m) => Id -> m (Maybe Entry)
load i = do
  mrow <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  loadImpl mrow

loadMetadata :: (MonadKorrvigs m) => Id -> m Metadata
loadMetadata i = do
  mtdt <- rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    mtdtRow <- selectTable entriesMetadataTable
    where_ $ mtdtRow ^. sqlEntry .== (entry ^. sqlEntryId)
    pure (mtdtRow ^. sqlKey, mtdtRow ^. sqlValue)
  pure $ M.fromList mtdt

dispatchRemove :: (MonadKorrvigs m) => (Int -> [Delete Int64] -> m ()) -> KindData -> m ()
dispatchRemove rm (LinkD lnk) =
  let i = lnk ^. linkEntry . entryId in rm i $ Link.sqlRemove i
dispatchRemove rm (NoteD note) =
  let i = note ^. noteEntry . entryId in rm i $ Note.sqlRemove i
dispatchRemove rm (FileD file) =
  let i = file ^. fileEntry . entryId in rm i $ File.sqlRemove i
dispatchRemove rm (EventD ev) =
  let i = ev ^. eventEntry . entryId in rm i $ Event.sqlRemove i
dispatchRemove rm (CalendarD cal) =
  let i = cal ^. calEntry . entryId in rm i $ Cal.sqlRemove i
dispatchRemove rm (SyndicateD syn) =
  let i = syn ^. synEntry . entryId in rm i $ Syn.sqlRemove i

removeKindDB :: (MonadKorrvigs m) => Entry -> m ()
removeKindDB entry = dispatchRemove (\_ dels -> atomicSQL $ \conn -> forM_ dels $ runDelete conn) $ entry ^. entryKindData

genRemoveDB :: (MonadKorrvigs m) => Int -> [Delete Int64] -> m ()
genRemoveDB i dels =
  atomicSQL $ \conn -> do
    forM_ dels $ runDelete conn
    void $
      runDelete conn $
        Delete
          { dTable = entriesMetadataTable,
            dWhere = \mrow -> mrow ^. sqlEntry .== sqlInt4 i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesSubTable,
            dWhere = \row -> row ^. source .== sqlInt4 i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesRefTable,
            dWhere = \row -> row ^. source .== sqlInt4 i,
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesTable,
            dWhere = \erow -> erow ^. sqlEntryId .== sqlInt4 i,
            dReturning = rCount
          }

removeDB :: (MonadKorrvigs m) => Entry -> m ()
removeDB entry = dispatchRemove genRemoveDB $ entry ^. entryKindData

data SyncData = SyncData
  { _syncEntryRow :: EntryRowW,
    _syncDataRows :: Int -> [Insert Int64],
    _syncMtdtRows :: [(CI Text, Value)],
    _syncTextContent :: Maybe Text,
    _syncTitle :: Maybe Text,
    _syncParents :: [Id],
    _syncRefs :: [Id],
    _syncCompute :: Map Text Action
  }

makeLenses ''SyncData

syncSQL :: (MonadKorrvigs m) => SyncData -> m ()
syncSQL dt = atomicSQL $ \conn -> do
  let i = dt ^. syncEntryRow . sqlEntryName
  -- Update entry
  mprev :: [EntryRowR] <- runSelect conn $ limit 1 $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure entry
  sqlI <- case listToMaybe mprev of
    Nothing -> do
      void $
        runInsert conn $
          Insert
            { iTable = entriesTable,
              iRows = [toFields $ dt ^. syncEntryRow],
              iReturning = rCount,
              iOnConflict = Just doNothing
            }
      sqlI <- runSelect conn $ limit 1 $ fromName pure $ sqlId i
      pure $ fromJust $ listToMaybe sqlI
    Just prev -> do
      let sqlI = prev ^. sqlEntryId
      case prev ^. sqlEntryKind of
        Link -> mapM_ (runDelete conn) $ Link.sqlRemove sqlI
        Note -> mapM_ (runDelete conn) $ Note.sqlRemove sqlI
        File -> mapM_ (runDelete conn) $ File.sqlRemove sqlI
        Event -> mapM_ (runDelete conn) $ Event.sqlRemove sqlI
        Calendar -> mapM_ (runDelete conn) $ Cal.sqlRemove sqlI
        Syndicate -> mapM_ (runDelete conn) $ Syn.sqlRemove sqlI
      let entryRow = dt ^. syncEntryRow & sqlEntryId ?~ sqlI
      void $
        runUpdate conn $
          Update
            { uTable = entriesTable,
              uUpdateWith = const $ toFields entryRow,
              uWhere = \row -> row ^. sqlEntryId .== sqlInt4 sqlI,
              uReturning = rCount
            }
      pure sqlI
  -- Insert into kind specific table
  forM_ (dt ^. syncDataRows $ sqlI) $ runInsert conn
  -- Optionally set textContent
  let mtdtVal = (^? _2 . _String) <$> filter (\r -> (r ^. _1) `S.member` indexedMetadata) (dt ^. syncMtdtRows)
  let txtContent = T.intercalate " " . mconcat $ toList <$> (dt ^. syncTextContent : dt ^. syncTitle : mtdtVal)
  unless (T.null txtContent) $ do
    -- Find language
    let lang = (^. _2) <$> find (\mrow -> mrow ^. _1 == mtdtName Language) (dt ^. syncMtdtRows)
    let parser = case lang of
          Just "fr" -> tsParseFrench
          _ -> tsParseEnglish
    -- Update
    void $
      runUpdate conn $
        Update
          { uTable = entriesTable,
            uUpdateWith = updateEasy $ sqlEntryText .~ toNullable (parser $ sqlStrictText txtContent),
            uWhere = \row -> row ^. sqlEntryId .== sqlInt4 sqlI,
            uReturning = rCount
          }
  -- Update metadata
  void $
    runDelete conn $
      Delete
        { dTable = entriesMetadataTable,
          dWhere = \mtdt -> mtdt ^. sqlEntry .== sqlInt4 sqlI,
          dReturning = rCount
        }
  void $
    runInsert conn $
      Insert
        { iTable = entriesMetadataTable,
          iRows = toFields . uncurry (MetadataRow sqlI) <$> dt ^. syncMtdtRows,
          iReturning = rCount,
          iOnConflict = Just doNothing
        }
  -- Update computations
  let compRows = uncurry (CompRow sqlI) <$> M.toList (dt ^. syncCompute)
  void $
    runDelete conn $
      Delete
        { dTable = computationsTable,
          dWhere = \comp -> comp ^. sqlCompEntry .== sqlInt4 sqlI,
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

syncRelsSQL :: (MonadKorrvigs m) => Int -> [Int] -> [Int] -> m ()
syncRelsSQL i subsOf relsTo = atomicSQL $ \conn -> do
  -- Sync parents
  void $
    runDelete conn $
      Delete
        { dTable = entriesSubTable,
          dWhere = \sb -> sb ^. source .== sqlInt4 i,
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
          dWhere = \sb -> sb ^. source .== sqlInt4 i,
          dReturning = rCount
        }
  unless (null relsTo) $
    void $
      runInsert conn $
        insertRefTo $
          (i,) <$> relsTo
