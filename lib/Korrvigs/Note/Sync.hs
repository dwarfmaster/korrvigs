module Korrvigs.Note.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad (forM_, void, when)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.FTS
import Korrvigs.Kind
import Korrvigs.KindData
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Helpers
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Render (writeNoteLazy)
import Korrvigs.Note.SQL
import Korrvigs.Utils.DateTree
import Opaleye
import System.Directory (doesFileExist, removeFile)
import System.FilePath (joinPath, takeBaseName)
import Prelude hiding (writeFile)

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

dRemoveDBImpl :: Id -> [Delete Int64]
dRemoveDBImpl i =
  [ Delete
      { dTable = notesTable,
        dWhere = \nrow -> nrow ^. sqlNoteName .== sqlId i,
        dReturning = rCount
      }
  ]

dRemoveImpl :: (MonadKorrvigs m) => FilePath -> m ()
dRemoveImpl path = do
  exists <- liftIO $ doesFileExist path
  when exists $ liftIO $ removeFile path

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

fromJSON' :: (FromJSON a) => Value -> Maybe a
fromJSON' v = case fromJSON v of
  Success x -> Just x
  Error _ -> Nothing

dSyncOneImpl :: (MonadKorrvigs m) => FilePath -> m RelData
dSyncOneImpl path = do
  let i = dGetIdImpl path
  prev <- load i
  forM_ prev dispatchRemoveDB
  doc <- readNote path >>= throwEither (KCantLoad i)
  syncDocument i path doc
  let parents = fromJSON' =<< doc ^. docMtdt . at (CI.mk "parents")
  pure $
    RelData
      { _relSubOf = maybe [] (fmap MkId) parents,
        _relRefTo = S.toList $ doc ^. docRefTo
      }

syncDocument :: (MonadKorrvigs m) => Id -> FilePath -> Document -> m ()
syncDocument i path doc = do
  let mtdt = doc ^. docMtdt
  let geom = fromJSON' =<< mtdt ^. at "geometry"
  let tm = fromJSON' =<< mtdt ^. at "date"
  let dur = fromJSON' =<< mtdt ^. at "duration"
  let erow = EntryRow i Note tm dur geom Nothing :: EntryRow
  let mrows = uncurry (MetadataRow i) <$> M.toList mtdt :: [MetadataRow]
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
      runInsert conn $
        Insert
          { iTable = entriesMetadataTable,
            iRows = toFields <$> mrows,
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

dUpdateImpl :: (MonadKorrvigs m) => Note -> (Document -> m Document) -> m ()
dUpdateImpl note f = do
  let path = note ^. notePath
  let i = note ^. noteEntry . name
  doc <- readNote path >>= throwEither (KCantLoad i)
  ndoc <- f doc
  liftIO $ writeFile path $ writeNoteLazy ndoc

dUpdateMetadataImpl :: (MonadKorrvigs m) => Note -> Map Text Value -> [Text] -> m ()
dUpdateMetadataImpl note upd rm = dUpdateImpl note $ pure . ndoc
  where
    updCi = M.fromList $ first CI.mk <$> M.toList upd
    rmCi = CI.mk <$> rm
    ndoc = docMtdt %~ M.union updCi . flip (foldr M.delete) rmCi

dUpdateParentsImpl :: (MonadKorrvigs m) => Note -> [Id] -> [Id] -> m ()
dUpdateParentsImpl note toAdd toRm = dUpdateImpl note $ pure . upd
  where
    updParents = foldr (.) id $ fmap S.insert toAdd ++ fmap S.delete toRm
    upd = docParents %~ updParents
