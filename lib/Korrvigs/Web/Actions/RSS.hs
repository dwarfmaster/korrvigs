module Korrvigs.Web.Actions.RSS where

import Control.Lens hiding (op, (.>))
import Control.Monad
import Data.Aeson.Lens hiding (values)
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.LocalTime
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Kind
import Korrvigs.Link.Download
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Monad.Sync
import Korrvigs.Query
import Korrvigs.Syndicate.Item
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.New
import qualified Korrvigs.Syndicate.Run as Syn
import Korrvigs.Syndicate.SQL
import qualified Korrvigs.Syndicate.Sync as Sync
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Opaleye
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Actions.New (parseSyndicateFilter)
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye
import qualified Opaleye as O
import Yesod hiding (Field, count)

--   ____                            _
--  | _ _|_ __ ___  _ __   ___  _ __| |_
--    | || '_ ` _ \| '_ \ / _ \| '__| __|
--    | || | | | | | |_) | (_) | |  | |_
--   |___|_| |_| |_| .__/ \___/|_|   \__|
--                 |_|
importRssTarget :: ActionTarget -> ActionCond
importRssTarget (TargetEntry _) = ActCondAlways
importRssTarget _ = ActCondNever

importRssForm :: AForm Handler Text
importRssForm = areq textField "URL" Nothing

importRssTitle :: ActionTarget -> Text
importRssTitle = const "Import RSS from"

runImportRSS :: Text -> ActionTarget -> Handler ActionReaction
runImportRSS url (TargetEntry entry) = do
  info <- flip appEndo def <$> downloadInformation url
  render <- getUrlRenderParams
  case info ^? neMtdt . at (mtdtName Feed) . _Just . _String of
    Nothing -> pure $ def & reactMsg ?~ [hamlet|<p>No feed found for <a href=#{url}>#{url}</a>|] render
    Just feed -> do
      updateMetadata entry (M.singleton (mtdtSqlName Feed) (toJSON feed)) []
      pure $ def & reactMsg ?~ [hamlet|<p>Found feed <a href=#{feed}>#{feed}</a>|] render
runImportRSS _ _ = pure def

--   ____                  _ _           _
--  / ___| _   _ _ __   __| (_) ___ __ _| |_ ___
--  \___ \| | | | '_ \ / _` | |/ __/ _` | __/ _ \
--   ___) | |_| | | | | (_| | | (_| (_| | ||  __/
--  |____/ \__, |_| |_|\__,_|_|\___\__,_|\__\___|
--          |___/
syndicateTarget :: ActionTarget -> ActionCond
syndicateTarget (TargetEntry _) =
  ActCondAnd
    [ ActCondQuery $ def & queryMtdt .~ [(mtdtSqlName Feed, TypeQuery JSIsText)],
      ActCondNot $ ActCondQuery $ def & queryMtdt .~ [(mtdtSqlName SyndicateMtdt, AnyQuery)]
    ]
syndicateTarget _ = ActCondNever

syndicateForm :: AForm Handler (Maybe Text, Bool)
syndicateForm =
  (,)
    <$> aopt textField "Filter" Nothing
    <*> areq checkBoxField "Run javascript" Nothing

syndicateTitle :: ActionTarget -> Text
syndicateTitle = const "Create syndicate"

runSyndicate :: (Maybe Text, Bool) -> ActionTarget -> Handler ActionReaction
runSyndicate (flt, runJS) (TargetEntry entry) = do
  feed <- rSelectOne (baseSelectTextMtdt Feed $ sqlInt4 $ entry ^. entryId) >>= throwMaybe (KMiscError $ "Entry " <> unId (entry ^. entryName) <> " has no feed metadata")
  let insertJS = if runJS then M.insert (mtdtName RunJavascript) (toJSON True) else id
  let ns = NewSyndicate (def & neParents .~ [entry ^. entryName] & neMtdt %~ insertJS) (Just feed) $ flt >>= parseSyndicateFilter
  i <- new ns
  updateMetadata entry (M.singleton (mtdtSqlName SyndicateMtdt) (toJSON $ unId i)) []
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ [hamlet|<p>Create syndicate <a href=@{EntryR $ WId i}>@#{unId i}</a>|] render
runSyndicate _ _ = pure def

--   ____
--  |  _ \ _   _ _ __
--  | |_) | | | | '_ \
--  |  _ <| |_| | | | |
--  |_| \_\\__,_|_| |_|
runSyndicateTarget :: ActionTarget -> ActionCond
runSyndicateTarget TargetHome = ActCondAlways
runSyndicateTarget (TargetEntry entry) = case entry ^? _Syndicate of
  Just syn -> if isJust (syn ^. synUrl) then ActCondAlways else ActCondNever
  Nothing -> ActCondQuery $ def & queryMentioning ?~ QueryRel queryIsSyn True
  where
    queryIsSyn = def & queryKind ?~ Syndicate
runSyndicateTarget _ = ActCondNever

runSyndicateForm :: AForm Handler Bool
runSyndicateForm = areq checkBoxField "Recursive" Nothing

runSyndicateTitle :: ActionTarget -> Text
runSyndicateTitle = const "Run syndication"

doRunSyndicate :: Syndicate -> Handler Bool
doRunSyndicate syn = do
  run <- Syn.run syn
  entries <- rSelect $ do
    e <- selectTable entriesTable
    syndicate <- baseSelectTextMtdt SyndicateMtdt $ e ^. sqlEntryId
    where_ $ syndicate .== sqlId (syn ^. synEntry . entryName)
    pure $ e ^. sqlEntryName
  forM_ entries $ load >=> mapM_ (`updateAggregate` syn)
  pure run

notArchived :: Field SqlInt4 -> Select ()
notArchived synId = do
  archived <- selectMtdt Archived synId
  where_ $ matchNullable (sqlBool True) O.not $ sqlJsonToBool archived

runRunSyndicate :: Bool -> ActionTarget -> Handler ActionReaction
runRunSyndicate _ TargetHome = do
  synIds <- rSelect $ do
    synId <- view sqlSynId <$> selectTable syndicatesTable
    notArchived synId
    pure synId
  doRunSyndicates synIds
runRunSyndicate rec (TargetEntry entry) = do
  react <- case entry ^. entryKindData of
    SyndicateD syn -> doRunSyndicates [syn ^. synEntry . entryId]
    _ -> do
      let select = if rec then selectRecTargetsFor else selectTargetsFor
      syns <- rSelect $ do
        ref <- select entriesRefTable $ sqlInt4 $ entry ^. entryId
        e <- selectTable entriesTable
        where_ $ e ^. sqlEntryId .== ref
        where_ $ e ^. sqlEntryKind .== sqlKind Syndicate
        notArchived ref
        pure ref
      doRunSyndicates syns
  render <- getUrlRenderParams
  let red = if entry ^. kind == Syndicate then Just (render (EntryR $ WId $ entry ^. entryName) []) else Nothing
  pure $
    react
      & reactRedirect .~ red
runRunSyndicate _ _ = pure def

doRunSyndicates :: [Int] -> Handler ActionReaction
doRunSyndicates synIds = do
  runResults <- fmap catMaybes <$> forM synIds $ \sqlI -> do
    entry <- loadSql sqlI
    forM (entry ^? _Just . _Syndicate) $ \syn ->
      (syn ^. synEntry . entryName,) <$> doRunSyndicate syn
  render <- getUrlRenderParams
  let msg =
        [hamlet|
      <ul>
        $forall (i,r) <- runResults
          <li>
            $if r
              Updated
              <a href=@{EntryR $ WId i}>
                @#{unId i}
            $else
              Nothing to do for
              <a href=@{EntryR $ WId i}>
                @#{unId i}
    |]
          render
  pure $ def & reactMsg ?~ msg

updateAggregate :: Entry -> Syndicate -> Handler ()
updateAggregate entry syn =
  rSelectTextMtdt AggregateMethod (sqlId $ entry ^. entryName) >>= \case
    Just "count-since-last" -> do
      mseq <- rSelectOne $ do
        (mtdtKey, isStrict) <-
          values
            [ (sqlStrictText $ mtdtSqlName LastRead, sqlBool True),
              (sqlStrictText $ mtdtSqlName FirstUnread, sqlBool False)
            ]
        mtdt <- selectTable entriesMetadataTable
        where_ $ mtdt ^. sqlEntry .== sqlInt4 sqlI
        where_ $ mtdt ^. sqlKey .== mtdtKey
        url <- fromNullableSelect $ pure $ sqlJsonToText $ toNullable $ mtdt ^. sqlValue
        item <- selectTable syndicatedItemsTable
        where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
        where_ $ item ^. sqlSynItUrl .== url
        pure (item ^. sqlSynItSequence, isStrict)
      forM_ mseq $ \(sq, isStrict) -> do
        mcnt :: Maybe Int64 <- rSelectOne $ aggregate count $ do
          item <- selectTable syndicatedItemsTable
          where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
          let op = if isStrict then (.>) else (.>=)
          where_ $ (item ^. sqlSynItSequence) `op` sqlInt4 sq
          pure $ item ^. sqlSynItSequence
        forM_ mcnt $ \cnt -> updateMetadata entry (M.singleton (mtdtSqlName AggregateCount) (toJSON cnt)) []
    Just "count-new" -> do
      mcnt :: Maybe Int64 <- rSelectOne $ aggregate count $ do
        item <- selectTable syndicatedItemsTable
        where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
        where_ $ isNull $ item ^. sqlSynItInstance
        where_ $ item ^. sqlSynItRead .== sqlBool False
        pure $ item ^. sqlSynItSequence
      forM_ mcnt $ \cnt -> updateMetadata entry (M.singleton (mtdtSqlName AggregateCount) (toJSON cnt)) []
    _ -> pure ()
  where
    sqlI = entry ^. entryId

--    ____            _                    _ _       _
--   / ___|__ _ _ __ | |_ _   _ _ __ ___  | (_)_ __ | | __
--  | |   / _` | '_ \| __| | | | '__/ _ \ | | | '_ \| |/ /
--  | |__| (_| | |_) | |_| |_| | | |  __/ | | | | | |   <
--   \____\__,_| .__/ \__|\__,_|_|  \___| |_|_|_| |_|_|\_\
--             |_|
data CapturedLink = CapturedLink
  { _clkTitle :: Maybe Text,
    _clkUrl :: Text,
    _clkDate :: Maybe LocalTime
  }

makeLenses ''CapturedLink

captureLinkTarget :: ActionTarget -> ActionCond
captureLinkTarget TargetHome = ActCondAlways
captureLinkTarget (TargetEntry entry) | entry ^. kind == Syndicate = ActCondAlways
captureLinkTarget _ = ActCondNever

captureLinkForm :: AForm Handler CapturedLink
captureLinkForm =
  CapturedLink
    <$> aopt textField "Title" Nothing
    <*> areq textField "URL" Nothing
    <*> aopt datetimeLocalField "Date" Nothing

captureLinkTitle :: ActionTarget -> Text
captureLinkTitle TargetHome = "Capture link"
captureLinkTitle _ = "Add link"

runCaptureLink :: CapturedLink -> ActionTarget -> Handler ActionReaction
runCaptureLink clk TargetHome = do
  entry <- load (MkId "CapturedLinks")
  render <- getUrlRenderParams
  case entry ^? _Just . _Syndicate of
    Just syn -> do
      doCaptureLink clk syn
      pure $ def & reactMsg ?~ [hamlet|<p>Capture successful.|] render
    Nothing -> pure $ def & reactMsg ?~ [hamlet|<p><code>CapturedLinks</code> syndicate does not exists.|] render
runCaptureLink clk (TargetEntry entry) = case entry ^. entryKindData of
  SyndicateD syn -> do
    doCaptureLink clk syn
    render <- getUrlRender
    pure $ def & reactRedirect ?~ render (EntryR $ WId $ syn ^. synEntry . entryName)
  _ -> pure def
runCaptureLink _ _ = pure def

doCaptureLink :: CapturedLink -> Syndicate -> Handler ()
doCaptureLink clk syn = do
  tm <- liftIO $ case clk ^. clkDate of
    Just local -> do
      tz <- getCurrentTimeZone
      pure $ localTimeToUTC tz local
    Nothing -> getCurrentTime
  let item =
        SyndicatedItem
          { _synitTitle = fromMaybe (clk ^. clkUrl) $ clk ^. clkTitle,
            _synitUrl = clk ^. clkUrl,
            _synitRead = False,
            _synitGUID = Nothing,
            _synitDate = Just tm,
            _synitInstance = Nothing
          }
  Sync.updateImpl syn $ pure . (synjsItems %~ (++ [item]))
  syncFileOfKind (syn ^. synPath) Syndicate
