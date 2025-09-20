module Korrvigs.Web.Actions.RSS where

import Control.Lens
import Data.Aeson.Lens
import Data.Default
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Link.Download
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Metadata
import Korrvigs.Query
import Korrvigs.Syndicate.New
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Opaleye
import Yesod

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
