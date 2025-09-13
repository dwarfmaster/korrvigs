{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Monad.Collections where

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Default
import Data.Foldable
import qualified Data.Map as M
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.File.SQL
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Task
import Korrvigs.Monad.Class
import Korrvigs.Monad.SQL
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Note.SQL
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Utils.Opaleye
import Opaleye hiding (Field)
import qualified Opaleye as O
import System.IO

data OptionalSQLDataImpl a b c d e = OptionalSQLData
  { _optSizeAction :: a,
    _optTask :: b,
    _optMime :: c,
    _optAggregCount :: d,
    _optCover :: e
  }

makeLenses ''OptionalSQLDataImpl
$(makeAdaptorAndInstanceInferrable "pOptSQLData" ''OptionalSQLDataImpl)

type OptionalSQLData = OptionalSQLDataImpl (Maybe Action) (Maybe Text) (Maybe Text) (Maybe Value) (Maybe Text)

type OptionalSQLDataSQL = OptionalSQLDataImpl (FieldNullable SqlJsonb) (FieldNullable SqlText) (MaybeFields (O.Field SqlText)) (FieldNullable SqlJsonb) (FieldNullable SqlText)

instance Default OptionalSQLData where
  def = OptionalSQLData Nothing Nothing Nothing Nothing Nothing

instance Default OptionalSQLDataSQL where
  def = OptionalSQLData O.null O.null O.nothingFields O.null O.null

optDef :: OptionalSQLDataSQL
optDef = def

otherQuery :: Collection -> EntryRowSQLR -> Select OptionalSQLDataSQL
otherQuery display entry = case display of
  ColGallery -> do
    (sz, mime) <- galleryQueryFor $ entry ^. sqlEntryId
    pure $
      optDef
        & optSizeAction .~ toNullable sz
        & optMime .~ mime
  ColNetwork -> pure optDef
  ColTaskList -> do
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryId
    agCount <- selectMtdt AggregateCount $ entry ^. sqlEntryId
    pure $ optDef & optTask .~ tsk & optAggregCount .~ agCount
  ColLibrary -> do
    cover <- baseSelectTextMtdt Cover (entry ^. sqlEntryId)
    coverId <- fromName pure cover
    (sz, mime) <- galleryQueryFor coverId
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryId
    pure $
      optDef
        & optCover .~ toNullable cover
        & optSizeAction .~ toNullable sz
        & optMime .~ mime
        & optTask .~ tsk
  _ -> do
    pure optDef
  where
    galleryQueryFor sqlI = do
      void $ selComp sqlI "miniature"
      sz <- selComp sqlI "size"
      mime <- optional $ do
        file <- selectTable filesTable
        where_ $ (file ^. sqlFileId) .== sqlI
        pure $ file ^. sqlFileMime
      pure (sz ^. sqlCompAction, mime)

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
  md <- hoistEitherLift $ readNote $ note ^. notePath
  guard $ col `M.member` (md ^. docCollections)
  let md' = md & docContent . each . bkCollection col . _3 %~ (++ [item])
  file <- liftIO $ openFile (note ^. notePath) WriteMode
  r <- lift $ writeNote file md'
  pure $ isNothing r

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
