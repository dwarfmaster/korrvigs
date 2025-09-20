module Korrvigs.Syndicate.Run where

import Conduit
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.XML.Types
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Sync
import Korrvigs.Syndicate.Item
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.Sync (updateImpl)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import
import Text.Feed.Types
import Text.HTML.TagSoup
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

-- Return Nothing if the ressource has expired or if it hasn't changed
lazyDownload :: (MonadKorrvigs m) => Text -> Maybe UTCTime -> Maybe Text -> m (Maybe (ByteString, Maybe Text))
lazyDownload url expiration etag = runMaybeT $ do
  forM_ expiration $ \expi -> do
    current <- liftIO getCurrentTime
    guard $ current > expi
  req' <- parseRequest $ T.unpack url
  let req = req' {requestHeaders = maybe [] (\etg -> [("If-None-Match", Enc.encodeUtf8 etg)]) etag ++ requestHeaders req'}
  man <- lift manager
  resp <- httpLbs req man
  let scode = statusCode $ responseStatus resp
  guard $ scode /= 304
  when (scode /= 200) $ throwM $ KMiscError $ "Error " <> T.pack (show scode) <> " when downloading " <> url
  let netag = lookup "ETag" $ responseHeaders resp
  pure (responseBody resp, Enc.decodeUtf8 <$> netag)

run :: (MonadKorrvigs m) => Syndicate -> m Bool
run syn =
  lazyDownload (syn ^. synUrl) (syn ^. synExpiration) (syn ^. synETag) >>= \case
    Nothing -> pure False
    Just (dat, netag) -> do
      feed <- throwMaybe (KMiscError "Failed to parse feed") $ parseFeedSource dat
      let setETag = synjsETag .~ netag
      current <- liftIO getCurrentTime
      let (imp, items) = case feed of
            AtomFeed fd -> importFromAtom fd
            RSSFeed fd -> importFromRSS current fd
            RSS1Feed fd -> importFromRSS1 fd
            XMLFeed _ -> (id, [])
      let updItems = synjsItems %~ mergeItemsInto items
      updateImpl syn $ pure . setETag . imp . updItems
      syncFileOfKind (syn ^. synPath) Syndicate
      pure True

mergeItemsInto :: [SyndicatedItem] -> [SyndicatedItem] -> [SyndicatedItem]
mergeItemsInto = foldr insertOneItem

insertOneItem :: SyndicatedItem -> [SyndicatedItem] -> [SyndicatedItem]
insertOneItem it = findAndInsert
  where
    isSame :: SyndicatedItem -> SyndicatedItem -> Bool
    isSame it1 it2 =
      let guid1 = it1 ^. synitGUID
       in let guid2 = it2 ^. synitGUID
           in (isJust guid1 && guid1 == guid2)
                || (isNothing guid1 && isNothing guid2 && it1 ^. synitUrl == it2 ^. synitUrl)
    merge :: SyndicatedItem -> SyndicatedItem -> SyndicatedItem
    merge new old = new & synitInstance .~ old ^. synitInstance
    findAndInsert :: [SyndicatedItem] -> [SyndicatedItem]
    findAndInsert [] = [it]
    findAndInsert (oit : oits) | isSame it oit = merge it oit : oits
    findAndInsert (oit : oits) = oit : findAndInsert oits

importFromAtom :: Atom.Feed -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromAtom feed = (setTitle . setAuthors, importFromEntry <$> Atom.feedEntries feed)
  where
    setTitle = synjsTitle ?~ extractText (Atom.feedTitle feed)
    setAuthors = synjsMetadata . at (mtdtSqlName Authors) ?~ toJSON (Atom.personName <$> Atom.feedAuthors feed)
    importFromEntry :: Atom.Entry -> SyndicatedItem
    importFromEntry entry =
      let dt = T.unpack $ Atom.entryUpdated entry
       in let date8601 = iso8601ParseM dt
           in let date822 = parseTimeM True defaultTimeLocale rfc822DateFormat dt
               in SyndicatedItem
                    { _synitTitle = extractText $ Atom.entryTitle entry,
                      _synitUrl = Atom.entryId entry,
                      _synitGUID = Nothing,
                      _synitDate = date8601 <|> date822,
                      _synitInstance = Nothing
                    }

extractText :: Atom.TextContent -> Text
extractText (Atom.TextString txt) = txt
extractText (Atom.HTMLString html) = T.intercalate " " $ mapMaybe ex $ parseTags html
  where
    ex (TagText txt) = Just txt
    ex _ = Nothing
extractText (Atom.XHTMLString xml) = T.intercalate " " $ exElem xml
  where
    exNode (NodeContent (ContentText txt)) = [txt]
    exNode (NodeElement el) = exElem el
    exNode _ = []
    exElem el = exNode =<< elementNodes el

importFromRSS :: UTCTime -> RSS.RSS -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromRSS time feed = (setTitle . setDesc . setTTL, mapMaybe importFromItem $ RSS.rssItems channel)
  where
    channel = RSS.rssChannel feed
    setTitle = synjsTitle ?~ RSS.rssTitle channel
    setDesc = synjsMetadata . at (mtdtSqlName Abstract) ?~ toJSON (RSS.rssDescription channel)
    setTTL = maybe id (\ttl -> synjsExpiration ?~ addUTCTime (fromInteger $ ttl * 60) time) $ RSS.rssTTL channel
    importFromItem :: RSS.RSSItem -> Maybe SyndicatedItem
    importFromItem item = do
      title <- RSS.rssItemTitle item <|> RSS.rssItemDescription item
      url <- RSS.rssItemLink item
      pure $
        SyndicatedItem
          { _synitTitle = title,
            _synitUrl = url,
            _synitGUID = RSS.rssGuidValue <$> RSS.rssItemGuid item,
            _synitDate = parseTimeM True defaultTimeLocale rfc822DateFormat . T.unpack =<< RSS.rssItemPubDate item,
            _synitInstance = Nothing
          }

importFromRSS1 :: RSS1.Feed -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromRSS1 feed = (setTitle . setDesc, importFromItem <$> RSS1.feedItems feed)
  where
    channel = RSS1.feedChannel feed
    setTitle = synjsTitle ?~ RSS1.channelTitle channel
    setDesc = synjsMetadata . at (mtdtSqlName Abstract) ?~ toJSON (RSS1.channelDesc channel)
    importFromItem :: RSS1.Item -> SyndicatedItem
    importFromItem item =
      SyndicatedItem
        { _synitTitle = RSS1.itemTitle item,
          _synitUrl = RSS1.itemURI item,
          _synitGUID = Nothing,
          _synitDate = Nothing,
          _synitInstance = Nothing
        }
