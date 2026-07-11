module Korrvigs.Web.Note.Syndicate (getNoteSyndicateR) where

import Control.Arrow
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Text (Text)
import Data.Time
import Korrvigs.Entry hiding (_Syndicate)
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Syndicate.SQL
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Opaleye hiding (groupBy, not)
import Text.Blaze
import Yesod

getNoteSyndicateR :: WebId -> Handler TypedContent
getNoteSyndicateR (WId i) = do
  accept <- lookupHeader "accept"
  onlyUnread <- isJust <$> lookupGetParam "unread"
  dat <- getData onlyUnread i
  render <- getUrlRenderParams
  if maybe False ("application/json" `BS.isPrefixOf`) accept
    then pure $ toTypedContent $ toJSON dat
    else fmap toTypedContent $ defaultLayout $ do
      Rcs.syndicates StaticR CssR
      setTitle $ "Syndicates for " <> toMarkup (unId i)
      [whamlet|
        $if onlyUnread
          <a href=@{NoteSyndicateR (WId i)}>
            With read
        $else
          <a href=#{render (NoteSyndicateR (WId i)) [("unread","")]}>
            Only unread
        <ul .syndicate-items>
          $forall item <- dat
            ^{renderItem item}
      |]

getData :: Bool -> Id -> Handler [(Id, Int, Maybe Text, [Text], Text, Text, Bool, Maybe UTCTime)]
getData onlyUnread i = do
  entry <- maybe notFound pure =<< load i
  note <- maybe notFound pure $ entry ^? _Note
  let path = note ^. notePath
  doc <- either (const notFound) pure =<< readNote path
  let syndicates =
        fmap doGroup $
          NE.groupBy (\(_, s1) (_, s2) -> s1 == s2) $
            sortBy (\(_, s1) (_, s2) -> compare s1 s2) $
              doc ^.. docContent . each . bkSubBlocks . _Syndicate . to prepSyn . each
  rSelect $ orderBy (descNullsFirst (view _8)) $ do
    (synId, tags) <- values $ (sqlId *** sqlArray sqlStrictText) <$> syndicates
    syn <- selectTable entriesTable
    where_ $ syn ^. sqlEntryName .== synId
    let sqlI = syn ^. sqlEntryId
    item <- limit 10 $ orderBy (desc (view sqlSynItSequence)) $ do
      item <- selectTable syndicatedItemsTable
      where_ $ item ^. sqlSynItSyndicate .== sqlI
      when onlyUnread $ where_ $ item ^. sqlSynItRead .== sqlBool False
      pure item
    pure (synId, item ^. sqlSynItSequence, syn ^. sqlEntryTitle, tags, item ^. sqlSynItTitle, item ^. sqlSynItUrl, item ^. sqlSynItRead, item ^. sqlSynItDate)
  where
    prepSyn (col, _, _, syns) = (col,) <$> syns
    doGroup ((col1, s) :| cols) = (s, col1 : (fst <$> cols))

renderItem :: (Id, Int, Maybe Text, [Text], Text, Text, Bool, Maybe UTCTime) -> Widget
renderItem (synId, sq, synTitle, tags, title, url, isRead, date) = do
  liId <- newIdent
  [whamlet|
      <li ##{liId} *{readClass}>
        <p>
          $if not isRead
            <span .item-read item-to-read=#{liId} item-read-url=@{SynItemReadR (WId synId) sq}>
              ✓
          <a href=#{url}>
            #{title}
          <a href=@{EntryR (WId synId)} .item-syn>
            $maybe t <- synTitle
              #{t}
            $nothing
              @#{unId synId}
        <p>
          $maybe dt <- date
            <span .item-date>
              #{formatTime defaultTimeLocale "%Y/%m/%d" dt}
          $forall tag <- tags
            <span .tag>
              #{tag}
    |]
  where
    readClass :: [(Text, Text)]
    readClass = [("class", "read") | isRead]
