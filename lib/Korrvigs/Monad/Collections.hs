{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Monad.Collections where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens hiding (like)
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.Aeson.Lens
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Monoid
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Data.Time
import Korrvigs.Compute.SQL
import Korrvigs.Entry
import Korrvigs.Entry.JSON
import Korrvigs.File.SQL
import Korrvigs.Geometry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Contact
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

data OptionalSQLDataImpl a b c d e = OptionalSQLData
  { _optTask :: a,
    _optMime :: b,
    _optAggregCount :: c,
    _optCover :: d,
    _optContact :: e
  }

makeLenses ''OptionalSQLDataImpl
$(makeAdaptorAndInstanceInferrable "pOptSQLData" ''OptionalSQLDataImpl)

type OptionalSQLData = OptionalSQLDataImpl (Maybe Text) (Maybe Text) (Maybe Value) (Maybe Text) (Maybe ContactDataRow)

type OptionalSQLDataSQL = OptionalSQLDataImpl (FieldNullable SqlText) (MaybeFields (O.Field SqlText)) (FieldNullable SqlJsonb) (FieldNullable SqlText) (MaybeFields ContactDataSQL)

instance Default OptionalSQLData where
  def = OptionalSQLData Nothing Nothing Nothing Nothing Nothing

instance Default OptionalSQLDataSQL where
  def = OptionalSQLData O.null O.nothingFields O.null O.null O.nothingFields

optDef :: OptionalSQLDataSQL
optDef = def

type ColEntry = EntryRowImpl Int (Maybe Kind) Id (Maybe ZonedTime) (Maybe CalendarDiffTime) (Maybe Geometry) (Maybe ()) (Maybe Text)

toColEntry :: EntryRowR -> ColEntry
toColEntry = sqlEntryKind %~ Just

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
    coverId <- selectEntryId cover
    mime <- galleryQueryFor coverId
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryId
    agCount <- selectMtdt AggregateCount $ entry ^. sqlEntryId
    pure $
      optDef
        & optCover .~ toNullable cover
        & optMime .~ mime
        & optTask .~ tsk
        & optAggregCount .~ agCount
  ColPlayList -> do
    file <- selectTable filesTable
    where_ $ file ^. sqlFileId .== (entry ^. sqlEntryId)
    let mime = file ^. sqlFileMime
    where_ $ (mime `like` sqlStrictText "audio/%") .|| (mime `like` sqlStrictText "video/%")
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryId
    pure $ optDef & optMime .~ justFields mime & optTask .~ tsk
  ColContacts -> do
    dat <- selectContactData entry
    pure $ optDef & optContact .~ justFields dat
  _ -> pure optDef

galleryQueryFor :: O.Field SqlInt4 -> Select (MaybeFields (O.Field SqlText))
galleryQueryFor sqlI = do
  void $ selComp sqlI "miniature"
  void $ selComp sqlI "size"
  optional $ do
    file <- selectTable filesTable
    where_ $ (file ^. sqlFileId) .== sqlI
    pure $ file ^. sqlFileMime

optDefPlain :: OptionalSQLData
optDefPlain = def

getMtdt :: (ExtraMetadata mtdt) => EntryJSON -> mtdt -> Getting (First a) Value a -> Maybe a
getMtdt entry m l = entry ^? ejsMetadata . at (mtdtSqlName m) . _Just . l

otherDummy :: (MonadKorrvigs m) => Collection -> EntryJSON -> MaybeT m OptionalSQLData
otherDummy ColGallery _ = mzero
otherDummy ColNetwork _ = pure def
otherDummy ColTaskList entry =
  pure $
    optDefPlain
      & optTask .~ getMtdt entry TaskMtdt _String
      & optAggregCount .~ getMtdt entry AggregateCount _Value
otherDummy ColLibrary entry = do
  cover <- hoistMaybe $ getMtdt entry Cover _String
  mime <- hoistLift $ rSelectOne $ do
    coverId <- selectEntryId $ MkId cover
    mime <- galleryQueryFor coverId
    fromNullableSelect $ pure $ maybeFieldsToNullable mime
  pure $
    optDefPlain
      & optCover ?~ cover
      & optMime ?~ mime
      & optTask .~ getMtdt entry TaskMtdt _String
      & optAggregCount .~ getMtdt entry AggregateCount _Value
otherDummy ColPlayList _ = mzero
otherDummy ColContacts entry =
  pure $ optDefPlain & optContact .~ otherContact entry
otherDummy _ _ = pure def

otherContact :: EntryJSON -> Maybe ContactDataRow
otherContact e = do
  nm <- e ^. ejsTitle <|> getMtdt e FullName _String
  pure $
    ContactData
      { _contactName = nm,
        _contactBirthDay = getMtdt e BirthDayMtdt id,
        _contactBirthYear = getMtdt e BirthYear id,
        _contactDeath = getMtdt e Death id,
        _contactContacts = getMtdt e ContactMtdt id,
        _contactGender = getMtdt e Gender id,
        _contactPronouns = getMtdt e Pronouns id,
        _contactNicknames = getMtdt e Nicknames id,
        _contactPicture = getMtdt e Cover id,
        _contactUrl = getMtdt e Url id
      }

runQuery :: (MonadKorrvigs m) => Collection -> Query -> m [(ColEntry, OptionalSQLData)]
runQuery display query =
  fmap (each . _1 %~ toColEntry) $ rSelect $ compile query $ otherQuery display

expandID :: (MonadKorrvigs m) => Collection -> Id -> m [(ColEntry, OptionalSQLData)]
expandID display i = do
  res <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    other <- otherQuery display entry
    pure (entry, other)
  pure $ toList $ res & _Just . _1 %~ toColEntry

loadCollection :: (MonadKorrvigs m) => Collection -> [CollectionItem] -> m [(ColEntry, OptionalSQLData)]
loadCollection = concatMapM . loadCollectionItem

noteCollection :: (MonadKorrvigs m) => Id -> Text -> m (Maybe [CollectionItem])
noteCollection i col = runMaybeT $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? entryKindData . _NoteD
  md <- hoistEitherLift $ readNote $ note ^. notePath
  hoistMaybe $ md ^? docContent . each . bkCollection col . _3

fromDummy :: (MonadKorrvigs m) => Collection -> EntryJSON -> m [(ColEntry, OptionalSQLData)]
fromDummy c js = toList . fmap (colRow,) <$> runMaybeT (otherDummy c js)
  where
    colRow :: ColEntry
    colRow = EntryRow 0 Nothing (MkId "") (js ^. ejsDate) (js ^. ejsDuration) (js ^. ejsGeo) Nothing (js ^. ejsTitle)

loadCollectionItem :: (MonadKorrvigs m) => Collection -> CollectionItem -> m [(ColEntry, OptionalSQLData)]
loadCollectionItem c (ColItemEntry i) = expandID c i
loadCollectionItem c (ColItemInclude i included) = fromMaybeT [] $ do
  col <- hoistMaybe =<< lift (noteCollection i included)
  lift $ loadCollection c col
loadCollectionItem c (ColItemQuery q) = runQuery c q
loadCollectionItem c (ColItemSubOf i) =
  runQuery c $ def & querySubOf ?~ QueryRel (def & queryId .~ [i]) False
loadCollectionItem c (ColItemDummy v) = fromDummy c v
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
    mSqlI :: Maybe Int <- rSelectOne $ selectEntryId $ sqlId colI
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
collectionsFor i = fmap (fromMaybe []) $ rSelectOne $ do
  sqlI <- selectEntryId i
  note <- selectTable notesTable
  where_ $ note ^. sqlNoteId .== sqlI
  pure $ note ^. sqlNoteCollections

capture :: (MonadKorrvigs m) => Id -> m Bool
capture = addToCollection (MkId "Favourites") "captured" . ColItemEntry
