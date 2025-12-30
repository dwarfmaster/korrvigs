module Korrvigs.Web.Search.Form
  ( queryForm,
    displayForm,
    applyPrefix,
    sortOptions,
    maxResultsOptions,
    displayResultOptions,
    getParameters,
  )
where

import Control.Arrow (first)
import Control.Lens
import Control.Monad
import Data.Default
import Data.List (find)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import Korrvigs.Entry
import qualified Korrvigs.FTS as FTS
import Korrvigs.Kind
import Korrvigs.Note.AST (Collection (..))
import Korrvigs.Query
import Korrvigs.Utils
import Korrvigs.Web.Backend
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
    opts = (,) <$> [ByTitle, ById, ByDate, ByDistanceTo (V2 0 0), ByTSRank (FTS.Phrase [])] <*> [SortAsc, SortDesc]
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
    dCrit ByTitle = "Title"
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

displayResultOptions :: [Option Collection]
displayResultOptions = mkOption <$> [minBound .. maxBound]
  where
    mkOption :: Collection -> Option Collection
    mkOption ColList = Option "list" ColList "1"
    mkOption ColMap = Option "map" ColMap "2"
    mkOption ColGallery = Option "gallery" ColGallery "3"
    mkOption ColTimeline = Option "timeline" ColTimeline "4"
    mkOption ColNetwork = Option "network" ColNetwork "5"
    mkOption ColFuzzy = Option "fuzzy" ColFuzzy "6"
    mkOption ColCalendar = Option "calendar" ColCalendar "7"
    mkOption ColKanban = Option "kanban" ColKanban "8"
    mkOption ColTaskList = Option "tasklist" ColTaskList "9"
    mkOption ColLibrary = Option "library" ColLibrary "10"

displayResultsField :: Field Handler Collection
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
    <$> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checktitle") <*> iopt textField (applyPrefix prefix "title"))
    <*> (getOpt <$> ireq checkBoxField (applyPrefix prefix "checkfts") <*> iopt ftsField (applyPrefix prefix "fts"))
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
    <*> kindQueryForm prefix
    <*> ( getOpt
            <$> ireq checkBoxField (applyPrefix prefix "checkmtdt")
            <*> (zip <$> ireq keysField (applyPrefix prefix "mtdtKey") <*> ireq valuesField (applyPrefix prefix "mtdtVal"))
        )
    <*> ( getOpt
            <$> ireq checkBoxField (applyPrefix prefix "checkincol")
            <*> ( mkInCol
                    <$> iopt textField (applyPrefix prefix "incolname")
                    <*> iopt textField (applyPrefix prefix "incolentry")
                )
        )
    <*> maybe (queryRelForm mktz "sub") (const $ pure Nothing) prefix
    <*> maybe (queryRelForm mktz "parent") (const $ pure Nothing) prefix
    <*> maybe (queryRelForm mktz "mentioning") (const $ pure Nothing) prefix
    <*> maybe (queryRelForm mktz "mentioned") (const $ pure Nothing) prefix
    <*> ireq checkBoxField (applyPrefix prefix "showhidden")
    <*> (fromMaybe (ByDate, SortDesc) <$> iopt optsField "sortopts")
    <*> (join <$> iopt maxResultsField "maxresults")
  where
    mkInCol :: Maybe Text -> Maybe Text -> Maybe QueryInCollection
    mkInCol (Just name) (Just i) = Just $ QueryInCol name $ MkId i
    mkInCol _ _ = Nothing

selectKindQuery ::
  Maybe Kind ->
  Maybe NoteQuery ->
  Maybe FileQuery ->
  Maybe EventQuery ->
  Maybe CalendarQuery ->
  Maybe SyndicateQuery ->
  Maybe KindQuery
selectKindQuery (Just Note) (Just nq) _ _ _ _ = Just $ KindQueryNote nq
selectKindQuery (Just File) _ (Just fq) _ _ _ = Just $ KindQueryFile fq
selectKindQuery (Just Event) _ _ (Just eq) _ _ = Just $ KindQueryEvent eq
selectKindQuery (Just Calendar) _ _ _ (Just cq) _ = Just $ KindQueryCalendar cq
selectKindQuery (Just Syndicate) _ _ _ _ (Just sq) = Just $ KindQuerySyndicate sq
selectKindQuery _ _ _ _ _ _ = Nothing

kindQueryForm :: Maybe Text -> FormInput Handler (Maybe KindQuery)
kindQueryForm prefix =
  getOpt
    <$> ireq checkBoxField (applyPrefix prefix "checkkind")
    <*> ( selectKindQuery
            <$> iopt kindField (applyPrefix prefix "kind")
            <*> pure (Just NoteQuery)
            <*> ( Just . FileQuery . joinNull T.null
                    <$> iopt textField (applyPrefix prefix "file-mime")
                )
            <*> pure (Just EventQuery)
            <*> pure (Just CalendarQuery)
            <*> ( Just . SyndicateQuery . joinNull T.null
                    <$> iopt textField (applyPrefix prefix "syn-url")
                )
        )

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

displayForm :: Collection -> FormInput Handler Collection
displayForm c = fromMaybe c <$> iopt displayResultsField "display"

getKindParameters :: Maybe Text -> KindQuery -> [(Text, Text)]
getKindParameters _ (KindQueryNote NoteQuery) = []
getKindParameters prefix (KindQueryFile fq) =
  maybe [] (\mime -> [(applyPrefix prefix "file-mime", mime)]) (fq ^. queryFileMime)
getKindParameters _ (KindQueryEvent EventQuery) = []
getKindParameters _ (KindQueryCalendar CalendarQuery) = []
getKindParameters prefix (KindQuerySyndicate sq) =
  maybe [] (\url -> [(applyPrefix prefix "syn-url", url)]) (sq ^. querySyndicateUrl)

getParameters :: Maybe Text -> Query -> Collection -> [(Text, Text)]
getParameters prefix q display =
  (first (applyPrefix prefix) <$> basicAttrs)
    ++ (if isJust prefix then [] else resAttrs ++ subAttrs ++ parentAttrs ++ mentioningAttrs ++ mentionedAttrs)
  where
    displayTime :: ZonedTime -> Text
    displayTime zt = T.pack $ formatTime defaultTimeLocale "%0Y-%m-%dT%H:%M" zt
    displayBool :: Bool -> Text
    displayBool True = "on"
    displayBool False = "off"
    mtdtAttrs :: (Text, JsonQuery) -> [(Text, Text)]
    mtdtAttrs (key, jsq) = [("mtdtKey", key), ("mtdtVal", renderJSQuery jsq)]
    basicAttrs =
      maybe [] (\tit -> [("checktitle", "on"), ("title", tit)]) (q ^. queryTitle)
        ++ maybe [] (\fts -> [("checkfts", "on"), ("fts", FTS.renderQuery fts)]) (q ^. queryText)
        ++ maybe [] (\bf -> [("checkdate", "on"), ("before", displayTime bf)]) (q ^. queryBefore)
        ++ maybe [] (\af -> maybe [("checkdate", "on")] (const []) (q ^. queryBefore) ++ [("after", displayTime af)]) (q ^. queryAfter)
        ++ maybe [] (\(V2 lng lat, dist) -> [("checkgeo", "on"), ("geolng", T.pack $ show lng), ("geolat", T.pack $ show lat), ("geodist", T.pack $ show $ dist / 1000.0)]) (q ^. queryDist)
        ++ maybe [] (\kd -> [("checkkind", "on"), ("kind", displayKind (queryToKind kd))] ++ getKindParameters prefix kd) (q ^. queryKind)
        ++ (if null (q ^. queryMtdt) then [] else ("checkmtdt", "on") : concatMap mtdtAttrs (q ^. queryMtdt))
        ++ maybe [] (\incol -> [("checkincol", "on"), ("incolname", incol ^. colName), ("incolentry", unId $ incol ^. colEntry)]) (q ^. queryInCollection)
        ++ [("showhidden", if q ^. queryShowHidden then "on" else "off")]
    relAttrs p = maybe [] (\r -> [(applyPrefix (Just p) "rec", displayBool $ r ^. relRec), (applyPrefix (Just p) "check", "on")] ++ getParameters (Just p) (r ^. relOther) display)
    subAttrs = relAttrs "sub" $ q ^. querySubOf
    parentAttrs = relAttrs "parent" $ q ^. queryParentOf
    mentioningAttrs = relAttrs "mentioning" $ q ^. queryMentioning
    mentionedAttrs = relAttrs "mentioned" $ q ^. queryMentionedBy
    sortAttr = maybe "1" optionExternalValue $ find (\opt -> optionInternalValue opt == q ^. querySort) sortOptions
    displayAttr = maybe "1" optionExternalValue $ find (\opt -> optionInternalValue opt == display) displayResultOptions
    resAttrs =
      [("sortopts", sortAttr), ("maxresults", maybe "nothing" (T.pack . show) (q ^. queryMaxResults)), ("display", displayAttr)]
