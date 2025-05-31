{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Monad.Collections where

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Foldable
import Data.Maybe
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import qualified Data.Set as S
import Data.Text (Text)
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Metadata
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

data OptionalSQLDataImpl a b c = OptionalSQLData
  { _optTitle :: a,
    _optSizeAction :: b,
    _optTask :: c
  }

makeLenses ''OptionalSQLDataImpl
$(makeAdaptorAndInstanceInferrable "pOptSQLData" ''OptionalSQLDataImpl)

type OptionalSQLData = OptionalSQLDataImpl (Maybe Text) (Maybe Action) (Maybe Text)

type OptionalSQLDataSQL = OptionalSQLDataImpl (FieldNullable SqlText) (FieldNullable SqlJsonb) (FieldNullable SqlText)

instance Default OptionalSQLData where
  def = OptionalSQLData Nothing Nothing Nothing

instance Default OptionalSQLDataSQL where
  def = OptionalSQLData O.null O.null O.null

optDef :: OptionalSQLDataSQL
optDef = def

otherQuery :: Collection -> EntryRowSQL -> Select OptionalSQLDataSQL
otherQuery display entry = case display of
  ColGallery -> do
    void $ selComp (entry ^. sqlEntryName) "miniature"
    sz <- selComp (entry ^. sqlEntryName) "size"
    pure $ optDef & optSizeAction .~ toNullable (sz ^. sqlCompAction)
  ColNetwork -> pure optDef
  ColTaskList -> do
    title <- selectTextMtdt Title $ entry ^. sqlEntryName
    tsk <- selectTextMtdt TaskMtdt $ entry ^. sqlEntryName
    pure $ optDef & optTitle .~ title & optTask .~ tsk
  _ -> do
    title <- selectTextMtdt Title $ entry ^. sqlEntryName
    pure $ optDef & optTitle .~ title

runQuery :: (MonadKorrvigs m) => Collection -> Query -> m [(EntryRow, OptionalSQLData)]
runQuery display query = rSelect $ do
  entry <- compile query
  other <- otherQuery display entry
  pure (entry, other)

expandID :: (MonadKorrvigs m) => Collection -> Id -> m [(EntryRow, OptionalSQLData)]
expandID display i = do
  res <- rSelectOne $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryName .== sqlId i
    other <- otherQuery display entry
    pure (entry, other)
  pure $ toList res

loadCollection :: (MonadKorrvigs m) => Collection -> [CollectionItem] -> m [(EntryRow, OptionalSQLData)]
loadCollection = concatMapM . loadCollectionItem

loadCollectionItem :: (MonadKorrvigs m) => Collection -> CollectionItem -> m [(EntryRow, OptionalSQLData)]
loadCollectionItem c (ColItemEntry i) = expandID c i
loadCollectionItem c (ColItemInclude i included) = fromMaybeT [] $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? kindData . _NoteD
  md <- hoistEitherLift $ readNote $ note ^. notePath
  col <- hoistMaybe $ md ^? docContent . each . bkCollection included . _3
  lift $ loadCollection c col
loadCollectionItem c (ColItemQuery q) = runQuery c q
loadCollectionItem c (ColItemSubOf i) =
  runQuery c $ def & querySubOf ?~ QueryRel (def & queryId .~ [i]) False
loadCollectionItem _ (ColItemComment _) = pure []

-- Returns False is the item could not be added
addToCollection :: (MonadKorrvigs m) => Id -> Text -> CollectionItem -> m Bool
addToCollection i col item = fromMaybeT False $ do
  entry <- hoistLift $ load i
  note <- hoistMaybe $ entry ^? kindData . _NoteD
  md <- hoistEitherLift $ readNote $ note ^. notePath
  guard $ col `S.member` (md ^. docCollections)
  let md' = md & docContent . each . bkCollection col . _3 %~ (++ [item])
  file <- liftIO $ openFile (note ^. notePath) WriteMode
  r <- lift $ writeNote file md'
  pure $ isNothing r

allCollections :: (MonadKorrvigs m) => m [(Id, Text)]
allCollections = rSelect $ do
  note <- selectTable notesTable
  col <- sqlUnnest $ note ^. sqlNoteCollections
  pure (note ^. sqlNoteName, col)

collectionsFor :: (MonadKorrvigs m) => Id -> m [Text]
collectionsFor i = fmap (fromMaybe []) $ rSelectOne $ do
  note <- selectTable notesTable
  where_ $ note ^. sqlNoteName .== sqlId i
  pure $ note ^. sqlNoteCollections

capture :: (MonadKorrvigs m) => Id -> m Bool
capture = addToCollection (MkId "Favourites") "captured" . ColItemEntry
