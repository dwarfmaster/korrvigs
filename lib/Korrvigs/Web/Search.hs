module Korrvigs.Web.Search (getSearchR) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import qualified Korrvigs.FTS as FTS
import Korrvigs.Kind
import Korrvigs.Query
import Korrvigs.Web.Backend
import Korrvigs.Web.Login
import qualified Korrvigs.Web.Ressources as Rcs
import Yesod

sattr :: Text -> Bool -> [(Text, Text)]
sattr attr True = [(attr, "")]
sattr _ False = []

mattr :: Maybe a -> Text -> (a -> Text) -> [(Text, Text)]
mattr (Just x) attr val = [(attr, val x)]
mattr Nothing _ _ = []

-- TODO render query as text
ftsForm :: Maybe FTS.Query -> Handler Widget
ftsForm fts =
  pure
    [whamlet|
      <details .search-group *{sattr "open" $ isJust fts}>
        <summary>
          <input type=checkbox name=checkfts *{sattr "checked" $ isJust fts}>
          Full text search
        <input type=text name=fts>
    |]

timeForm :: Maybe ZonedTime -> Maybe ZonedTime -> Handler Widget
timeForm after before =
  pure
    [whamlet|
      <details .search-group *{sattr "open" anyJust}>
        <summary>
          <input type=checkbox name=checkdate *{sattr "checked" anyJust}>
          Time
        After:
        <input type=datetime-local name=after *{afterVal}>
        <br>
        Before:
        <input type=datetime-local name=before *{beforeVal}>
    |]
  where
    anyJust = isJust after || isJust before
    afterVal = mattr after "value" $ T.pack . iso8601Show . zonedTimeToLocalTime
    beforeVal = mattr before "value" $ T.pack . iso8601Show . zonedTimeToLocalTime

kindForm :: Maybe Kind -> Handler Widget
kindForm kd =
  pure
    [whamlet|
    <details .search-group *{sattr "open" $ isJust kd}>
      <summary>
        <input type=checkbox name=checkkind *{sattr "checked" $ isJust kd}>
        Kind
      <select name=kind>
        $forall skd <- kinds
          <option value=#{displayKind skd} *{sattr "selected" $ Just skd == kd}>
            #{displayKind skd}
  |]
  where
    kinds :: [Kind]
    kinds = [minBound .. maxBound]

sortOptions :: [Option (SortCriterion, SortOrder)]
sortOptions = zipWith mkOption [1 ..] opts
  where
    opts = (,) <$> [ById, ByDate] <*> [SortAsc, SortDesc]
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

sortForm :: (SortCriterion, SortOrder) -> Handler Widget
sortForm sopt =
  pure
    [whamlet|
      <select name=sortopts>
        $forall opt <- sortOptions
          <option value=#{optionExternalValue opt} *{sattr "selected" $ optionInternalValue opt == sopt}>
            Sort by:
            #{optionDisplay opt}
    |]

maxResultsOptions :: [Option Int]
maxResultsOptions = mkOption <$> opts
  where
    opts = [10, 25, 50]
    mkOption :: Int -> Option Int
    mkOption n =
      Option
        { optionDisplay = T.pack $ show n,
          optionInternalValue = n,
          optionExternalValue = T.pack $ show n
        }

maxResultsForm :: Maybe Int -> Handler Widget
maxResultsForm n =
  pure
    [whamlet|
      <select name=maxresults>
        $forall opt <- maxResultsOptions
          <option value=#{optionExternalValue opt} *{sattr "selected" $ Just (optionInternalValue opt) == n}>
            Max results:
            #{optionDisplay opt}
    |]

searchForm :: Query -> Handler Widget
searchForm query = do
  fts <- ftsForm $ query ^. queryText
  time <- timeForm (query ^. queryAfter) (query ^. queryBefore)
  kd <- kindForm $ query ^. queryKind
  srt <- sortForm $ query ^. querySort
  mx <- maxResultsForm $ query ^. queryMaxResults
  pure $ do
    Rcs.formsStyle
    [whamlet|
      <form action=@{SearchR}>
        ^{fts}
        ^{time}
        ^{kd}
        ^{srt}
        ^{mx}
        <input .search-button type=submit value="Search">
    |]

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

kindField :: Field Handler Kind
kindField = selectField $ pure $ mkOptionList $ mkOption <$> [minBound .. maxBound]
  where
    mkOption kd =
      Option
        { optionDisplay = displayKind kd,
          optionInternalValue = kd,
          optionExternalValue = displayKind kd
        }

optsField :: Field Handler (SortCriterion, SortOrder)
optsField = selectField $ pure $ mkOptionList sortOptions

maxResultsField :: Field Handler Int
maxResultsField = selectField $ pure $ mkOptionList maxResultsOptions

getOpt :: Bool -> Maybe a -> Maybe a
getOpt False _ = Nothing
getOpt True x = x

getSearchR :: Handler Html
getSearchR = do
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  q <-
    runInputGet $
      Query
        <$> (getOpt <$> ireq checkBoxField "checkfts" <*> iopt ftsField "fts")
        <*> (getOpt <$> ireq checkBoxField "checkdate" <*> (mktz <$> iopt datetimeLocalField "before"))
        <*> (getOpt <$> ireq checkBoxField "checkdate" <*> (mktz <$> iopt datetimeLocalField "after"))
        <*> pure Nothing
        <*> pure Nothing
        <*> (getOpt <$> ireq checkBoxField "checkkind" <*> iopt kindField "kind")
        <*> pure []
        <*> (fromMaybe def <$> iopt optsField "sortopts")
        <*> iopt maxResultsField "maxresults"
  logWrap $ defaultLayout =<< searchForm q
