module Korrvigs.Web.Actions.RSS where

import Control.Lens hiding ((.>))
import Control.Monad
import Data.Aeson.Lens
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Kind
import Korrvigs.Link.Download
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Query
import Korrvigs.Syndicate.New
import qualified Korrvigs.Syndicate.Run as Syn
import Korrvigs.Syndicate.SQL
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye
import Yesod hiding (count)

--  ___                            _

-- | _ _|_ __ ___  _ __   ___  _ __| |_
--   | || '_ ` _ \| '_ \ / _ \| '__| __|
--   | || | | | | | |_) | (_) | |  | |_
--  |___|_| |_| |_| .__/ \___/|_|   \__|
--                |_|
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

--  ____                  _ _           _
-- / ___| _   _ _ __   __| (_) ___ __ _| |_ ___
-- \___ \| | | | '_ \ / _` | |/ __/ _` | __/ _ \
--  ___) | |_| | | | | (_| | | (_| (_| | ||  __/

-- | ____/ \__, |_| |_|\__,_|_|\___\__,_|\__\___|
--         |___/
syndicateTarget :: ActionTarget -> ActionCond
syndicateTarget (TargetEntry _) =
  ActCondAnd
    [ ActCondQuery $ def & queryMtdt .~ [(mtdtSqlName Feed, TypeQuery JSIsText)],
      ActCondNot $ ActCondQuery $ def & queryMtdt .~ [(mtdtSqlName SyndicateMtdt, AnyQuery)]
    ]
syndicateTarget _ = ActCondNever

syndicateForm :: AForm Handler ()
syndicateForm = pure ()

syndicateTitle :: ActionTarget -> Text
syndicateTitle = const "Create syndicate"

runSyndicate :: () -> ActionTarget -> Handler ActionReaction
runSyndicate () (TargetEntry entry) = do
  feed <- rSelectOne (baseSelectTextMtdt Feed $ sqlInt4 $ entry ^. entryId) >>= throwMaybe (KMiscError $ "Entry " <> unId (entry ^. entryName) <> " has no feed metadata")
  let ns = NewSyndicate (def & neParents .~ [entry ^. entryName]) feed Nothing
  i <- new ns
  updateMetadata entry (M.singleton (mtdtSqlName SyndicateMtdt) (toJSON $ unId i)) []
  render <- getUrlRenderParams
  pure $ def & reactMsg ?~ [hamlet|<p>Create syndicate <a href=@{EntryR $ WId i}>@#{unId i}|] render
runSyndicate () _ = pure def

--   ____
--  |  _ \ _   _ _ __
--  | |_) | | | | '_ \
--  |  _ <| |_| | | | |
--  |_| \_\\__,_|_| |_|
runSyndicateTarget :: ActionTarget -> ActionCond
runSyndicateTarget TargetHome = ActCondAlways
runSyndicateTarget (TargetEntry entry) | entry ^. kind == Syndicate = ActCondAlways
runSyndicateTarget (TargetEntry _) = ActCondQuery $ def & queryMentioning ?~ QueryRel queryIsSyn False
  where
    queryIsSyn = def & queryKind ?~ Syndicate
runSyndicateTarget _ = ActCondNever

runSyndicateForm :: AForm Handler ()
runSyndicateForm = pure ()

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

runRunSyndicate :: () -> ActionTarget -> Handler ActionReaction
runRunSyndicate () TargetHome = do
  synIds <- rSelect $ view sqlSynId <$> selectTable syndicatesTable
  doRunSyndicates synIds
runRunSyndicate () (TargetEntry entry) = do
  react <- case entry ^. entryKindData of
    SyndicateD syn -> doRunSyndicates [syn ^. synEntry . entryId]
    _ -> do
      syns <- rSelect $ do
        ref <- selectTable entriesRefTable
        where_ $ ref ^. source .== sqlInt4 (entry ^. entryId)
        e <- selectTable entriesTable
        where_ $ e ^. sqlEntryId .== (ref ^. target)
        where_ $ e ^. sqlEntryKind .== sqlKind Syndicate
        pure $ ref ^. target
      doRunSyndicates syns
  render <- getUrlRenderParams
  let red = if entry ^. kind == Syndicate then Just (render (EntryR $ WId $ entry ^. entryName) []) else Nothing
  pure $
    react
      & reactRedirect .~ red
runRunSyndicate () _ = pure def

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
              Updated <a href=@{EntryR $ WId i}>@#{unId i}
            $else
              Nothing to do for <a href=@{EntryR $ WId i}>@#{unId i}
    |]
          render
  pure $ def & reactMsg ?~ msg

updateAggregate :: Entry -> Syndicate -> Handler ()
updateAggregate entry syn =
  rSelectTextMtdt AggregateMethod (sqlId $ entry ^. entryName) >>= \case
    Just "count-since-last" -> do
      mseq <- rSelectOne $ do
        lastread <- baseSelectTextMtdt LastRead $ sqlInt4 sqlI
        item <- selectTable syndicatedItemsTable
        where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
        where_ $ item ^. sqlSynItUrl .== lastread
        pure $ item ^. sqlSynItSequence
      forM_ mseq $ \sq -> do
        mcnt :: Maybe Int64 <- rSelectOne $ aggregate count $ do
          item <- selectTable syndicatedItemsTable
          where_ $ item ^. sqlSynItSyndicate .== sqlInt4 (syn ^. synEntry . entryId)
          where_ $ item ^. sqlSynItSequence .> sqlInt4 sq
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
