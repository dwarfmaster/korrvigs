module Korrvigs.Note.Sync where

import Control.Arrow (first, (&&&))
import Control.Lens
import Control.Monad (when)
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
import Korrvigs.Actions.SQL
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Helpers
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Render (writeNoteLazy)
import Korrvigs.Note.SQL
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree
import System.Directory (doesFileExist)
import System.FilePath (joinPath, takeBaseName)
import Prelude hiding (writeFile)

noteIdFromPath :: FilePath -> Id
noteIdFromPath = MkId . T.pack . takeBaseName

remove :: (MonadKorrvigs m) => Note -> m ()
remove note = do
  let path = note ^. notePath
  rt <- noteDirectory
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

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

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allNotes

sync :: (MonadKorrvigs m) => m (Map Id (SyncData NoteRow, EntryComps))
sync =
  M.fromList <$> (allNotes >>= mapM (sequence . (noteIdFromPath &&& syncOne)))

fromJSON' :: (FromJSON a) => Value -> Maybe a
fromJSON' v = case fromJSON v of
  Success x -> Just x
  Error _ -> Nothing

syncOne :: (MonadKorrvigs m) => FilePath -> m (SyncData NoteRow, EntryComps)
syncOne path = do
  let i = noteIdFromPath path
  doc <- readNote path >>= throwEither (KCantLoad i)
  dt <- syncDocument i path doc
  pure (dt, M.empty)

syncDocument :: (MonadKorrvigs m) => Id -> FilePath -> Document -> m (SyncData NoteRow)
syncDocument i path doc = do
  let mtdt = doc ^. docMtdt
  let geom = fromJSON' =<< mtdt ^. at "geometry"
  let tm = fromJSON' =<< mtdt ^. at "date"
  let dur = fromJSON' =<< mtdt ^. at "duration"
  let erow = EntryRow i Note tm dur geom Nothing :: EntryRow
  let mrows = uncurry (MetadataRow i) <$> M.toList mtdt :: [MetadataRow]
  let nrow = NoteRow i path :: NoteRow
  let txt = renderDocument doc
  pure $ SyncData erow nrow mrows (Just txt) (S.toList $ doc ^. docParents) (S.toList $ doc ^. docRefTo)

updateImpl :: (MonadKorrvigs m) => Note -> (Document -> m Document) -> m ()
updateImpl note f = do
  let path = note ^. notePath
  let i = note ^. noteEntry . name
  doc <- readNote path >>= throwEither (KCantLoad i)
  ndoc <- f doc
  liftIO $ writeFile path $ writeNoteLazy ndoc

updateMetadata :: (MonadKorrvigs m) => Note -> Map Text Value -> [Text] -> m ()
updateMetadata note upd rm = updateImpl note $ pure . ndoc
  where
    updCi = M.fromList $ first CI.mk <$> M.toList upd
    rmCi = CI.mk <$> rm
    ndoc = docMtdt %~ M.union updCi . flip (foldr M.delete) rmCi

updateParents :: (MonadKorrvigs m) => Note -> [Id] -> [Id] -> m ()
updateParents note toAdd toRm = updateImpl note $ pure . upd
  where
    updParents = foldr (.) id $ fmap S.insert toAdd ++ fmap S.delete toRm
    upd = docParents %~ updParents

listCompute :: (MonadKorrvigs m) => Note -> m EntryComps
listCompute _ = pure M.empty
