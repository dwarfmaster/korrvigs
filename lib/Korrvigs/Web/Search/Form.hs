module Korrvigs.Web.Search.Form
  ( queryForm,
    applyPrefix,
    sortOptions,
    maxResultsOptions,
    displayResultOptions,
    displayResultsField,
  )
where

import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import qualified Korrvigs.FTS as FTS
import Korrvigs.Kind
import Korrvigs.Query
import Korrvigs.Web.Backend
import Korrvigs.Web.Search.Results
import Linear.V2
import Yesod

applyPrefix :: Maybe Text -> Text -> Text
applyPrefix Nothing nm = nm
applyPrefix (Just prefix) nm = prefix <> "_" <> nm

ftsField :: Field Handler FTS.Query
ftsField =
  Field
    { fieldParse = \rawVals _ ->
        case rawVals of
          [""] -> pure $ Right Nothing
          [query] -> pure $ case FTS.parseQuery query of
            Left err -> Left $ SomeMessage err
            Right q -> Right $ Just q
          _ -> pure $ Right Nothing,
      fieldView = \idAttr nameAttr otherAttrs _ _ ->
        [whamlet|<input id=#{idAttr} name=#{nameAttr} *{otherAttrs} type=text>|],
      fieldEnctype = UrlEncoded
    }

keysField :: Field Handler [Text]
keysField =
  Field
    { fieldParse = \rawVals _ -> pure $ Right $ Just rawVals,
      fieldView = \_ _ _ _ _ -> mempty,
      fieldEnctype = UrlEncoded
    }

valuesField :: Field Handler [JsonQuery]
valuesField =
  Field
    { fieldParse = \rawVals _ ->
        case mapM parseJSQuery rawVals of
          Left err -> pure $ Left $ SomeMessage err
          Right qs -> pure $ Right $ Just qs,
      fieldView = \_ _ _ _ _ -> mempty,
      fieldEnctype = UrlEncoded
    }

kindField :: Field Handler Kind
kindField = radioField' $ pure $ mkOptionList $ mkOption <$> [minBound .. maxBound]
  where
    mkOption kd =
      Option
        { optionDisplay = displayKind kd,
          optionInternalValue = kd,
          optionExternalValue = displayKind kd
        }

sortOptions :: [Option (SortCriterion, SortOrder)]
sortOptions = zipWith mkOption [1 ..] opts
  where
    opts = (,) <$> [ById, ByDate, ByDistanceTo (V2 0 0), ByTSRank (FTS.Phrase [])] <*> [SortAsc, SortDesc]
    mkOption :: Int -> (SortCriterion, SortOrder) -> Option (SortCriterion, SortOrder)
    mkOption i (crit, order) =
      Option
        { optionDisplay = dCrit crit <> " " <> dOrd order,
          optionInternalValue = (crit, order),
          optionExternalValue = T.pack $ show i
        }
    dCrit ByDate = "Date"
    dCrit ById = "Id"
    dCrit (ByTSRank _) = "Text rank"
    dCrit (ByDistanceTo _) = "Distance"
    dOrd SortAsc = "ascending"
    dOrd SortDesc = "descending"

optsField :: Field Handler (SortCriterion, SortOrder)
optsField = selectField $ pure $ mkOptionList sortOptions

maxResultsOptions :: [Option (Maybe Int)]
maxResultsOptions = mkOption <$> opts
  where
    opts = [Just 10, Just 25, Just 50, Nothing]
    mkOption :: Maybe Int -> Option (Maybe Int)
    mkOption (Just n) =
      Option
        { optionDisplay = T.pack $ show n,
          optionInternalValue = Just n,
          optionExternalValue = T.pack $ show n
        }
    mkOption Nothing =
      Option
        { optionDisplay = "all",
          optionInternalValue = Nothing,
          optionExternalValue = "nothing"
        }

maxResultsField :: Field Handler (Maybe Int)
maxResultsField = selectField $ pure $ mkOptionList maxResultsOptions

displayResultOptions :: [Option ResultDisplay]
displayResultOptions = mkOption <$> [minBound .. maxBound]
  where
    mkOption :: ResultDisplay -> Option ResultDisplay
    mkOption DisplayList = Option "list" DisplayList "1"
    mkOption DisplayMap = Option "map" DisplayMap "2"
    mkOption DisplayGraph = Option "graph" DisplayGraph "3"
    mkOption DisplayTimeline = Option "timeline" DisplayTimeline "4"
    mkOption DisplayGallery = Option "gallery" DisplayGallery "5"
    mkOption DisplayFuzzy = Option "fuzzy" DisplayFuzzy "6"
    mkOption DisplayCalendar = Option "calendar" DisplayCalendar "7"

displayResultsField :: Field Handler ResultDisplay
displayResultsField = selectField $ pure $ mkOptionList displayResultOptions

getOpt :: (Default a) => Bool -> a -> a
getOpt False _ = def
getOpt True x = x

liftMaybe :: Bool -> (V2 (Maybe Double), Maybe Double) -> Maybe (V2 Double, Double)
liftMaybe True (V2 (Just x) (Just y), Just d) = Just (V2 x y, d * 1000.0)
liftMaybe _ _ = Nothing

queryForm :: (Maybe LocalTime -> Maybe ZonedTime) -> Maybe Text -> FormInput Handler Query
queryForm mktz prefix =
  Query []
    <$> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkfts") <*> iopt ftsField (applyPrefix prefix "fts"))
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkdate") <*> (mktz <$> iopt datetimeLocalField (applyPrefix prefix "before")))
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkdate") <*> (mktz <$> iopt datetimeLocalField (applyPrefix prefix "after")))
    <*> pure Nothing
    <*> ( liftMaybe
            <$> ireq checkBoxField (applyPrefix prefix "checkgeo")
            <*> ( (,)
                    <$> (V2 <$> iopt doubleField (applyPrefix prefix "geolng") <*> iopt doubleField (applyPrefix prefix "geolat"))
                    <*> iopt doubleField (applyPrefix prefix "geodist")
                )
        )
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkkind") <*> iopt kindField (applyPrefix prefix "kind"))
    <*> ( getOpt
            <$> ireq checkBoxField (applyPrefix prefix "checkmtdt")
            <*> (zip <$> ireq keysField (applyPrefix prefix "mtdtKey") <*> ireq valuesField (applyPrefix prefix "mtdtVal"))
        )
    <*> pure []
    <*> maybe (queryRelForm mktz "sub") (const $ pure Nothing) prefix
    <*> maybe (queryRelForm mktz "parent") (const $ pure Nothing) prefix
    <*> pure Nothing
    <*> pure Nothing
    <*> (fromMaybe (ByDate, SortDesc) <$> iopt optsField "sortopts")
    <*> (join <$> iopt maxResultsField "maxresults")

queryRelForm :: (Maybe LocalTime -> Maybe ZonedTime) -> Text -> FormInput Handler (Maybe QueryRel)
queryRelForm mktz prefix =
  getOpt
    <$> ireq checkBoxField (applyPrefix (Just prefix) "check")
    <*> ( Just
            <$> ( QueryRel
                    <$> queryForm mktz (Just prefix)
                    <*> ireq checkBoxField (applyPrefix (Just prefix) "rec")
                )
        )
