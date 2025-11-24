{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Monad.Collections where

import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Korrvigs.Compute.SQL
import Korrvigs.Entry
import Korrvigs.File.SQL
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Task
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import Korrvigs.Note
import Korrvigs.Note.AST
import qualified Korrvigs.Note.Pandoc as Pandoc
import Korrvigs.Note.SQL
import qualified Korrvigs.Note.Sync as Note
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Utils.Opaleye
import Opaleye hiding (Field)
import qualified Opaleye as O

data OptionalSQLDataImpl a b c d = OptionalSQLData
  { _optTask :: a,
    _optMime :: b,
    _optAggregCount :: c,
    _optCover :: d
  }

makeLenses ''OptionalSQLDataImpl
$(makeAdaptorAndInstanceInferrable "pOptSQLData" ''OptionalSQLDataImpl)

type OptionalSQLData = OptionalSQLDataImpl (Maybe Text) (Maybe Text) (Maybe Value) (Maybe Text)

type OptionalSQLDataSQL = OptionalSQLDataImpl (FieldNullable SqlText) (MaybeFields (O.Field SqlText)) (FieldNullable SqlJsonb) (FieldNullable SqlText)

instance Default OptionalSQLData where
  def = OptionalSQLData Nothing Nothing Nothing Nothing

instance Default OptionalSQLDataSQL where
  def = OptionalSQLData O.null O.nothingFields O.null O.null

optDef :: OptionalSQLDataSQL
optDef = def

otherQuery :: Collection -> EntryRowSQLR -> Select OptionalSQLDataSQL
otherQuery display entry = case display of
  ColGallery -> do
    mime <- galleryQueryFor $ entry ^. sqlEntryId
    pure $
      optDef
        & optMime .~ mime
  ColNetwork -> pure optDef
  ColTaskList -> do
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryId
    agCount <- selectMtdt AggregateCount $ entry ^. sqlEntryId
    pure $ optDef & optTask .~ tsk & optAggregCount .~ agCount
  ColLibrary -> do
    cover <- baseSelectTextMtdt Cover (entry ^. sqlEntryId)
    coverId <- fromName pure cover
    mime <- galleryQueryFor coverId
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryId
    pure $
      optDef
        & optCover .~ toNullable cover
        & optMime .~ mime
        & optTask .~ tsk
  _ -> do
    pure optDef
  where
    galleryQueryFor sqlI = do
      void $ selComp sqlI "miniature"
      void $ selComp sqlI "size"
      optional $ do
        file <- selectTable filesTable
        where_ $ (file ^. sqlFileId) .== sqlI
        pure $ file ^. sqlFileMime

runQuery :: (MonadKorrvigs m) => Collection -> Query -> m [(EntryRowR, OptionalSQLData)]
runQuery display query = rSelect $ compile query $ otherQuery display

expandID :: (MonadKorrvigs m) => Collection -> Id -> m [(EntryRowR, OptionalSQLData)]
expandID display i = do
  res <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    other <- otherQuery display entry
    pure (entry, other)
  pure $ toList res

loadCollection :: (MonadKorrvigs m) => Collection -> [CollectionItem] -> m [(EntryRowR, OptionalSQLData)]
loadCollection = concatMapM . loadCollectionItem

noteCollection :: (MonadKorrvigs m) => Id -> Text -> m (Maybe [CollectionItem])
noteCollection i col = runMaybeT $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? entryKindData . _NoteD
  md <- hoistEitherLift $ readNote $ note ^. notePath
  hoistMaybe $ md ^? docContent . each . bkCollection col . _3

loadCollectionItem :: (MonadKorrvigs m) => Collection -> CollectionItem -> m [(EntryRowR, OptionalSQLData)]
loadCollectionItem c (ColItemEntry i) = expandID c i
loadCollectionItem c (ColItemInclude i included) = fromMaybeT [] $ do
  col <- hoistMaybe =<< lift (noteCollection i included)
  lift $ loadCollection c col
loadCollectionItem c (ColItemQuery q) = runQuery c q
loadCollectionItem c (ColItemSubOf i) =
  runQuery c $ def & querySubOf ?~ QueryRel (def & queryId .~ [i]) False
loadCollectionItem _ (ColItemComment _) = pure []

-- Returns False is the item could not be added
addToCollection :: (MonadKorrvigs m) => Id -> Text -> CollectionItem -> m Bool
addToCollection i col item = fromMaybeT False $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? entryKindData . _NoteD
  let doUpdate = docContent . each . bkCollection col . _3 %~ (++ [item])
  let checkForCol = anyOf (docContent . each . bkCollection col . _3) (const True)
  r <- lift $ Note.updateImpl' note $ pure . (doUpdate &&& checkForCol)
  forM_ (Pandoc.extractItem item) $ \colI -> lift $ do
    mSqlI :: Maybe Int <- rSelectOne $ fromName pure $ sqlId colI
    forM_ mSqlI $ \sqlI -> atomicSQL $ \conn ->
      runInsert conn $
        Insert
          { iTable = entriesRefTable,
            iRows = [toFields $ RelRow (entry ^. entryId) sqlI],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  pure r

allCollections :: (MonadKorrvigs m) => m [(Id, Text)]
allCollections = rSelect $ do
  entry <- selectTable entriesTable
  note <- selectTable notesTable
  where_ $ entry ^. sqlEntryId .== (note ^. sqlNoteId)
  col <- sqlUnnest $ note ^. sqlNoteCollections
  pure (entry ^. sqlEntryName, col)

collectionsFor :: (MonadKorrvigs m) => Id -> m [Text]
collectionsFor i = fmap (fromMaybe []) $ rSelectOne $ flip fromName (sqlId i) $ \sqlI -> do
  note <- selectTable notesTable
  where_ $ note ^. sqlNoteId .== sqlI
  pure $ note ^. sqlNoteCollections

capture :: (MonadKorrvigs m) => Id -> m Bool
capture = addToCollection (MkId "Favourites") "captured" . ColItemEntry
