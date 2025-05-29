{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Actions.Collections where

import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Default
import Data.Foldable
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Korrvigs.Actions.SQL
import Korrvigs.Compute.Action
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Query
import Korrvigs.Utils
import Opaleye hiding (Field)
import qualified Opaleye as O

data OptionalSQLDataImpl a b = OptionalSQLData
  { _optTitle :: a,
    _optSizeAction :: b
  }

makeLenses ''OptionalSQLDataImpl
$(makeAdaptorAndInstanceInferrable "pOptSQLData" ''OptionalSQLDataImpl)

type OptionalSQLData = OptionalSQLDataImpl (Maybe Text) (Maybe Action)

type OptionalSQLDataSQL = OptionalSQLDataImpl (FieldNullable SqlText) (FieldNullable SqlJsonb)

instance Default OptionalSQLData where
  def = OptionalSQLData Nothing Nothing

instance Default OptionalSQLDataSQL where
  def = OptionalSQLData O.null O.null

optDef :: OptionalSQLDataSQL
optDef = def

otherQuery :: Collection -> EntryRowSQL -> Select OptionalSQLDataSQL
otherQuery display entry = case display of
  ColGallery -> do
    void $ selComp (entry ^. sqlEntryName) "miniature"
    sz <- selComp (entry ^. sqlEntryName) "size"
    pure $ optDef & optSizeAction .~ toNullable (sz ^. sqlCompAction)
  ColNetwork -> pure optDef
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
loadCollectionItem _ (ColItemComment _) = pure []
