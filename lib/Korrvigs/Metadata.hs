module Korrvigs.Metadata where

import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Types
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.JSON (fromJSONM, sqlJsonToText)
import Opaleye
import qualified Opaleye as O

-- Helper types
newtype JSONList a = JSONList {unJSList :: [a]}
  deriving (Eq, Ord, Show)

instance (ToJSON a) => ToJSON (JSONList a) where
  toJSON = toJSON . unJSList

instance (FromJSON a) => FromJSON (JSONList a) where
  parseJSON js@(Array _) = JSONList <$> parseJSON js
  parseJSON (String "") = pure $ JSONList []
  parseJSON Null = pure $ JSONList []
  parseJSON invalid =
    prependFailure "parsing list failed, " $ typeMismatch "Array" invalid

-- Metadata functions
class ExtraMetadata mtdt where
  type MtdtType mtdt :: Type
  mtdtName :: mtdt -> CI Text
  mtdtSqlName :: mtdt -> Text
  mtdtSqlName = CI.foldedCase . mtdtName

extractMtdt ::
  (ExtraMetadata mtdt, FromJSON (MtdtType mtdt)) =>
  mtdt ->
  Map (CI Text) Value ->
  Maybe (MtdtType mtdt)
extractMtdt mtdt mp = M.lookup (mtdtName mtdt) mp >>= fromJSONM

insertMtdt ::
  (ExtraMetadata mtdt, ToJSON (MtdtType mtdt)) =>
  mtdt ->
  MtdtType mtdt ->
  Map (CI Text) Value ->
  Map (CI Text) Value
insertMtdt mtdt val = M.insert (mtdtName mtdt) $ toJSON val

baseSelectMtdt :: (ExtraMetadata mtdt) => mtdt -> Field SqlText -> Select (Field SqlJsonb)
baseSelectMtdt mtdt i = do
  m <- selectTable entriesMetadataTable
  where_ $ (m ^. sqlEntry) .== i
  where_ $ m ^. sqlKey .== sqlStrictText (mtdtSqlName mtdt)
  pure $ m ^. sqlValue

rSelectMtdt ::
  (ExtraMetadata mtdt, FromJSON (MtdtType mtdt), MonadKorrvigs m) =>
  mtdt ->
  Field SqlText ->
  m (Maybe (MtdtType mtdt))
rSelectMtdt mtdt i =
  rSelectOne (baseSelectMtdt mtdt i) <&> \case
    Nothing -> Nothing
    Just js -> case fromJSON js of
      Success v -> v
      Error _ -> Nothing

selectMtdt :: (ExtraMetadata mtdt) => mtdt -> Field SqlText -> Select (FieldNullable SqlJsonb)
selectMtdt mtdt i =
  fmap maybeFieldsToNullable $ optional $ limit 1 $ baseSelectMtdt mtdt i

baseSelectTextMtdt :: (ExtraMetadata mtdt, MtdtType mtdt ~ Text) => mtdt -> Field SqlText -> Select (FieldNullable SqlText)
baseSelectTextMtdt mtdt i = do
  m <- selectTable entriesMetadataTable
  where_ $ (m ^. sqlEntry) .== i
  where_ $ m ^. sqlKey .== sqlStrictText (CI.foldedCase $ mtdtName mtdt)
  pure $ sqlJsonToText $ toNullable $ m ^. sqlValue

rSelectTextMtdt ::
  (ExtraMetadata mtdt, MtdtType mtdt ~ Text, MonadKorrvigs m) =>
  mtdt ->
  Field SqlText ->
  m (Maybe Text)
rSelectTextMtdt mtdt i = rSelectOne (baseSelectTextMtdt mtdt i) <&> join

selectTextMtdt :: (ExtraMetadata mtdt, MtdtType mtdt ~ Text) => mtdt -> Field SqlText -> Select (FieldNullable SqlText)
selectTextMtdt mtdt i = fmap joinMField $ optional $ limit 1 $ baseSelectTextMtdt mtdt i
  where
    joinMField :: MaybeFields (FieldNullable a) -> FieldNullable a
    joinMField mfield = matchMaybe mfield $ \case
      Just f -> f
      Nothing -> O.null

-- Metadata list
data Title = Title

instance ExtraMetadata Title where
  type MtdtType Title = Text
  mtdtName = const "title"

data Language = Language

instance ExtraMetadata Language where
  type MtdtType Language = Text
  mtdtName = const "language"

data Favourite = Favourite

instance ExtraMetadata Favourite where
  type MtdtType Favourite = JSONList Text
  mtdtName = const "favourite"

data Pages = Pages

instance ExtraMetadata Pages where
  type MtdtType Pages = Int
  mtdtName = const "pages"

data Height = Height

instance ExtraMetadata Height where
  type MtdtType Height = Int
  mtdtName = const "height"

data Width = Width

instance ExtraMetadata Width where
  type MtdtType Width = Int
  mtdtName = const "width"

data Gallery = Gallery

instance ExtraMetadata Gallery where
  type MtdtType Gallery = Text
  mtdtName = const "gallery"
