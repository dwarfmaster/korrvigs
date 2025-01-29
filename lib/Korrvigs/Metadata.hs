module Korrvigs.Metadata where

import Control.Lens
import Control.Monad (join)
import Data.Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.JSON (sqlJsonToText)
import Opaleye
import qualified Opaleye as O

-- Metadata functions
class ExtraMetadata mtdt where
  mtdtName :: mtdt -> CI Text
  mtdtSqlName :: mtdt -> Text
  mtdtSqlName = CI.foldedCase . mtdtName

type family MtdtType mtdt

extractMtdt ::
  (ExtraMetadata mtdt, FromJSON (MtdtType mtdt)) =>
  mtdt ->
  Map (CI Text) Value ->
  Maybe (MtdtType mtdt)
extractMtdt mtdt mp = do
  value <- M.lookup (mtdtName mtdt) mp
  case fromJSON value of
    Success v -> pure v
    Error _ -> Nothing

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
  mtdtName = const "title"

type instance MtdtType Title = Text

data Language = Language

instance ExtraMetadata Language where
  mtdtName = const "language"

type instance MtdtType Language = Text

data Favourite = Favourite

instance ExtraMetadata Favourite where
  mtdtName = const "favourite"

type instance MtdtType Favourite = [Text]

data Pages = Pages

instance ExtraMetadata Pages where
  mtdtName = const "pages"

type instance MtdtType Pages = Int

data Height = Height

instance ExtraMetadata Height where
  mtdtName = const "height"

type instance MtdtType Height = Int

data Width = Width

instance ExtraMetadata Width where
  mtdtName = const "width"

type instance MtdtType Width = Int

data Gallery = Gallery

instance ExtraMetadata Gallery where
  mtdtName = const "gallery"

type instance MtdtType Gallery = Text
