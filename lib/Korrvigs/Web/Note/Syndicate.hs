module Korrvigs.Web.Note.Syndicate (getNoteSyndicateR, getNoteSyndicateSingleR) where

import Control.Arrow
import Control.Lens
import Control.Monad
import qualified Data.ByteString as BS
import Data.Hashable
import Data.List
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Korrvigs.Entry hiding (_Syndicate)
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Note.AST
import Korrvigs.Syndicate.SQL
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Opaleye hiding (groupBy, not, null)
import System.Random
import Text.Blaze
import Yesod

getNoteSyndicateR :: WebId -> Handler TypedContent
getNoteSyndicateR (WId i) = getSyndicateImpl i Nothing

getNoteSyndicateSingleR :: WebId -> Text -> Handler TypedContent
getNoteSyndicateSingleR (WId i) tag = getSyndicateImpl i (Just tag)

getSyndicateImpl :: Id -> Maybe Text -> Handler TypedContent
getSyndicateImpl i mtag = do
  accept <- lookupHeader "accept"
  onlyUnread <- isJust <$> lookupGetParam "unread"
  dat <- getData onlyUnread mtag i
  render <- getUrlRenderParams
  if maybe False ("application/json" `BS.isPrefixOf`) accept
    then pure $ toTypedContent $ toJSON dat
    else fmap toTypedContent $ defaultLayout $ do
      Rcs.syndicates StaticR CssR
      setTitle $ "Syndicates for " <> toMarkup (unId i)
      [whamlet|
        <p>
          <a href=@{EntryR (WId i)}>
            Parent entry
          $if onlyUnread
            <a href=@{selfRoute}>
              With read
          $else
            <a href=#{render selfRoute [("unread","")]}>
              Only unread
          $if isJust mtag
            $if onlyUnread
              <a href=#{render (NoteSyndicateR (WId i)) [("unread", "")]}>
                All tags
            $else
              <a href=@{NoteSyndicateR (WId i)}>
                All tags
        <ul .syndicate-items>
          $forall item <- dat
            ^{renderItem i onlyUnread item}
      |]
  where
    selfRoute = case mtag of
      Nothing -> NoteSyndicateR (WId i)
      Just tag -> NoteSyndicateSingleR (WId i) tag

getData :: Bool -> Maybe Text -> Id -> Handler [(Id, Int, Maybe Text, [Text], Text, Text, Bool, Maybe UTCTime)]
getData onlyUnread mtag i = do
  entry <- maybe notFound pure =<< load i
  note <- maybe notFound pure $ entry ^? _Note
  let path = note ^. notePath
  doc <- either (const notFound) pure =<< readNote path
  let syndicates =
        fmap doGroup $
          NE.groupBy (\(_, s1) (_, s2) -> s1 == s2) $
            sortBy (\(_, s1) (_, s2) -> compare s1 s2) $
              doc ^.. docContent . each . bkSubBlocks . _Syndicate . synFilter . to prepSyn . each
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
    synFilter = case mtag of
      Nothing -> id
      Just tag -> filtered ((== tag) . view _1)

renderItem :: Id -> Bool -> (Id, Int, Maybe Text, [Text], Text, Text, Bool, Maybe UTCTime) -> Widget
renderItem curId onlyUnread (synId, sq, synTitle, tags, title, url, isRead, date) = do
  render <- getUrlRenderParams
  liId <- newIdent
  [whamlet|
      $newline never
      <li ##{liId} *{readClass}>
        <p>
          $if not isRead
            <span .item-read item-to-read=#{liId} item-read-url=@{SynItemReadR (WId synId) sq}>
              ✓
          $if starred
            ★
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
          $forall tag <- showedTags
            <span .tag *{tagStyle tag}>
              $if onlyUnread
                <a href=#{render (NoteSyndicateSingleR (WId curId) tag) [("unread","")]}>
                  #{tag}
              $else
                <a href=@{NoteSyndicateSingleR (WId curId) tag}>
                  #{tag}
  |]
  where
    classes :: [Text]
    classes = ["read" | isRead] <> ["starred" | starred]
    readClass :: [(Text, Text)]
    readClass = [("class", T.intercalate " " classes) | not (null classes)]
    tagSet = S.fromList tags
    showedTags = S.difference tagSet $ S.fromList ["star"]
    starred = "star" `S.member` tagSet
    tagStyle :: Text -> [(Text, Text)]
    tagStyle tg = [("style", "background-color: " <> cssColor (tagColor tg))]

-- Returns an integer between 1 and 8 included
tagColor :: Text -> Int
tagColor tag = fst $ uniformR (1, 8) $ mkStdGen $ hash tag

cssColor :: Int -> Text
cssColor 1 = "var(--base08)"
cssColor 2 = "var(--base09)"
cssColor 3 = "var(--base0A)"
cssColor 4 = "var(--base0B)"
cssColor 5 = "var(--base0C)"
cssColor 6 = "var(--base0D)"
cssColor 7 = "var(--base0E)"
cssColor _ = "var(--base0F)"
