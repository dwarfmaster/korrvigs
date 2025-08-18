module Korrvigs.Metadata where

import Control.Arrow (first)
import Control.Lens
import Data.Aeson
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Metadata.TH
import Korrvigs.Monad.Class
import Korrvigs.Utils.JSON (fromJSONM, sqlJsonToText)
import Korrvigs.Utils.Opaleye (fromNullableSelect)
import Opaleye
import qualified Opaleye as O

-- Metadata functions
class ExtraMetadata mtdt where
  type MtdtType mtdt :: Type
  mtdtName :: mtdt -> CI Text
  mtdtSqlName :: mtdt -> Text
  mtdtSqlName = CI.foldedCase . mtdtName

unCIMtdt :: Metadata -> Map Text Value
unCIMtdt = M.fromList . fmap (first CI.foldedCase) . M.toList

reCIMtdt :: Map Text Value -> Metadata
reCIMtdt = M.fromList . fmap (first CI.mk) . M.toList

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

rSelectListMtdt ::
  (ExtraMetadata mtdt, FromJSON (MtdtType mtdt), MonadKorrvigs m, MtdtType mtdt ~ [a]) =>
  mtdt ->
  Field SqlText ->
  m (MtdtType mtdt)
rSelectListMtdt mtdt i = fromMaybe [] <$> rSelectMtdt mtdt i

selectMtdt :: (ExtraMetadata mtdt) => mtdt -> Field SqlText -> Select (FieldNullable SqlJsonb)
selectMtdt mtdt i =
  fmap maybeFieldsToNullable $ optional $ limit 1 $ baseSelectMtdt mtdt i

baseSelectTextMtdt :: (ExtraMetadata mtdt, MtdtType mtdt ~ Text) => mtdt -> Field SqlText -> Select (Field SqlText)
baseSelectTextMtdt mtdt i = fromNullableSelect $ do
  m <- selectTable entriesMetadataTable
  where_ $ (m ^. sqlEntry) .== i
  where_ $ m ^. sqlKey .== sqlStrictText (CI.foldedCase $ mtdtName mtdt)
  pure $ sqlJsonToText $ toNullable $ m ^. sqlValue

rSelectTextMtdt ::
  (ExtraMetadata mtdt, MtdtType mtdt ~ Text, MonadKorrvigs m) =>
  mtdt ->
  Field SqlText ->
  m (Maybe Text)
rSelectTextMtdt mtdt i = rSelectOne (baseSelectTextMtdt mtdt i)

selectTextMtdt :: (ExtraMetadata mtdt, MtdtType mtdt ~ Text) => mtdt -> Field SqlText -> Select (FieldNullable SqlText)
selectTextMtdt mtdt i = fmap joinMField $ optional $ limit 1 $ baseSelectTextMtdt mtdt i
  where
    joinMField :: MaybeFields (Field a) -> FieldNullable a
    joinMField mfield = matchMaybe mfield $ \case
      Just f -> toNullable f
      Nothing -> O.null

-- Metadata list
mkMtdt "Title" "title" [t|Text|]
mkMtdt "Language" "language" [t|Text|]
mkMtdt "Authors" "authors" [t|[Text]|]
mkMtdt "Pages" "pages" [t|Int|]
mkMtdt "Height" "height" [t|Int|]
mkMtdt "Width" "width" [t|Int|]
mkMtdt "Gallery" "gallery" [t|Text|]
mkMtdt "Private" "private" [t|Text|]
mkMtdt "HubMtdt" "hub" [t|Text|]
