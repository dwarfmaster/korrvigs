module Korrvigs.Syndicate.Run where

import Conduit
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU8
import Data.Conduit.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Data.XML.Types
import Korrvigs.Compute.Runnable (runInOut)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Sync
import Korrvigs.Note.Code (codeRunnable)
import Korrvigs.Syndicate.Item
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.New (lazyUpdateDate, lookupFromUrl)
import Korrvigs.Syndicate.Sync (updateImpl)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Network.URI
import System.Exit
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import
import Text.Feed.Types
import Text.HTML.TagSoup
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1

-- Return Nothing if the ressource has expired or if it hasn't changed
lazyDownload ::
  (MonadIO m, MonadThrow m, MonadResource m) =>
  Manager ->
  Text ->
  Maybe UTCTime ->
  Maybe Text ->
  m (Maybe (ConduitT () BSU8.ByteString m (), Maybe Text))
lazyDownload man url expiration etag = runMaybeT $ do
  forM_ expiration $ \expi -> do
    current <- liftIO getCurrentTime
    guard $ current > expi
  req' <- parseRequest $ T.unpack url
  let req = req' {requestHeaders = maybe [] (\etg -> [("If-None-Match", Enc.encodeUtf8 etg)]) etag ++ requestHeaders req'}
  resp <- lift $ http req man
  let scode = statusCode $ responseStatus resp
  guard $ scode /= 304
  when (scode /= 200) $ throwM $ KMiscError $ "Error " <> T.pack (show scode) <> " when downloading " <> url
  let netag = lookup "ETag" $ responseHeaders resp
  pure (responseBody resp, Enc.decodeUtf8 <$> netag)

run :: (MonadKorrvigs m, MonadResource m) => Syndicate -> m Bool
run syn = do
  man <- manager
  lazyDownload man (syn ^. synUrl) (syn ^. synExpiration) (syn ^. synETag) >>= \case
    Nothing -> pure False
    Just (dat, netag) -> do
      (imp, items) <- case syn ^. synFilter of
        Nothing -> runWithoutFilter dat
        Just flt -> runWithFilter dat flt
      let setETag = synjsETag .~ netag
      let updItems synjs = do
            nitems <- mergeItemsInto items $ synjs ^. synjsItems
            pure $ synjs & synjsItems .~ nitems
      updateImpl syn $ updItems . setETag . imp
      syncFileOfKind (syn ^. synPath) Syndicate
      pure True

runWithFilter ::
  (MonadKorrvigs m, MonadResource m) =>
  ConduitT () BSU8.ByteString m () ->
  (Id, Text) ->
  m (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
runWithFilter dat (i, code) =
  codeRunnable i code >>= \case
    Nothing -> pure (id, [])
    Just rbl -> do
      (exit, items) <- runInOut rbl dat $ conduitArray .| sinkList
      case exit of
        ExitSuccess -> pure (id, items)
        ExitFailure _ -> pure (id, [])

runWithoutFilter ::
  (MonadKorrvigs m, MonadResource m) =>
  ConduitT () BSU8.ByteString m () ->
  m (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
runWithoutFilter dat = do
  lbs <- runConduit $ dat .| sinkLazy
  feed <- throwMaybe (KMiscError "Failed to parse feed") $ parseFeedSource lbs
  current <- liftIO getCurrentTime
  pure $ case feed of
    AtomFeed fd -> importFromAtom fd
    RSSFeed fd -> importFromRSS current fd
    RSS1Feed fd -> importFromRSS1 fd
    XMLFeed _ -> (id, [])

mergeItemsInto :: (MonadKorrvigs m) => [SyndicatedItem] -> [SyndicatedItem] -> m [SyndicatedItem]
mergeItemsInto = flip (foldM (flip insertOneItem)) . reverse

insertOneItem :: (MonadKorrvigs m) => SyndicatedItem -> [SyndicatedItem] -> m [SyndicatedItem]
insertOneItem it = findAndInsert
  where
    merge :: (MonadKorrvigs m) => SyndicatedItem -> SyndicatedItem -> m SyndicatedItem
    merge new old =
      tryInstantiate $ new & synitInstance .~ old ^. synitInstance
    findAndInsert :: (MonadKorrvigs m) => [SyndicatedItem] -> m [SyndicatedItem]
    findAndInsert [] = (: []) <$> tryInstantiate it
    findAndInsert (oit : oits) | isSame it oit = (: oits) <$> merge it oit
    findAndInsert (oit : oits) = (:) <$> tryInstantiate oit <*> findAndInsert oits

tryInstantiate :: (MonadKorrvigs m) => SyndicatedItem -> m SyndicatedItem
tryInstantiate it = case it ^. synitInstance of
  Just _ -> pure it
  Nothing -> do
    inst <- lookupFromUrl $ it ^. synitUrl
    forM_ inst $ flip lazyUpdateDate $ it ^. synitDate
    pure $ it & synitInstance .~ inst

parseDate :: Text -> Maybe UTCTime
parseDate date =
  iso8601ParseM dt
    <|> (zonedTimeToUTC <$> iso8601ParseM dt)
    <|> parseTimeM True defaultTimeLocale rfc822DateFormat dt
  where
    dt = T.unpack date

-- Remove utm_* parameters
normalizeURL :: Text -> Text
normalizeURL url = case parseURI $ T.unpack url of
  Nothing -> url
  Just uri ->
    let q = parseQuery $ BS8.pack $ uriQuery uri
     in let nq = filter (not . BS.isPrefixOf "utm_" . fst) q
         in T.pack $ show $ uri {uriQuery = BSU8.toString $ renderQuery True nq, uriFragment = ""}

importFromAtom :: Atom.Feed -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromAtom feed = (setTitle . setAuthors, mapMaybe importFromEntry $ Atom.feedEntries feed)
  where
    setTitle = synjsTitle %~ Just . fromMaybe (extractText $ Atom.feedTitle feed)
    setAuthors = synjsMetadata . at (mtdtSqlName Authors) ?~ toJSON (Atom.personName <$> Atom.feedAuthors feed)
    importFromEntry :: Atom.Entry -> Maybe SyndicatedItem
    importFromEntry entry = do
      let dt = fromMaybe (Atom.entryUpdated entry) $ Atom.entryPublished entry
      url <- Atom.linkHref <$> listToMaybe (Atom.entryLinks entry)
      pure $
        SyndicatedItem
          { _synitTitle = extractText $ Atom.entryTitle entry,
            _synitUrl = normalizeURL url,
            _synitRead = False,
            _synitGUID = Just $ Atom.entryId entry,
            _synitDate = parseDate dt,
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
    setTitle = synjsTitle %~ Just . fromMaybe (RSS.rssTitle channel)
    setDesc = synjsMetadata . at (mtdtSqlName Abstract) ?~ toJSON (RSS.rssDescription channel)
    setTTL = maybe id (\ttl -> synjsExpiration ?~ addUTCTime (fromInteger $ ttl * 60) time) $ RSS.rssTTL channel
    importFromItem :: RSS.RSSItem -> Maybe SyndicatedItem
    importFromItem item = do
      title <- RSS.rssItemTitle item <|> RSS.rssItemDescription item
      url <- RSS.rssItemLink item
      pure $
        SyndicatedItem
          { _synitTitle = title,
            _synitUrl = normalizeURL url,
            _synitRead = False,
            _synitGUID = RSS.rssGuidValue <$> RSS.rssItemGuid item,
            _synitDate = parseDate =<< RSS.rssItemPubDate item,
            _synitInstance = Nothing
          }

importFromRSS1 :: RSS1.Feed -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromRSS1 feed = (setTitle . setDesc, importFromItem <$> RSS1.feedItems feed)
  where
    channel = RSS1.feedChannel feed
    setTitle = synjsTitle %~ Just . fromMaybe (RSS1.channelTitle channel)
    setDesc = synjsMetadata . at (mtdtSqlName Abstract) ?~ toJSON (RSS1.channelDesc channel)
    importFromItem :: RSS1.Item -> SyndicatedItem
    importFromItem item =
      SyndicatedItem
        { _synitTitle = RSS1.itemTitle item,
          _synitUrl = normalizeURL $ RSS1.itemURI item,
          _synitRead = False,
          _synitGUID = Nothing,
          _synitDate = Nothing,
          _synitInstance = Nothing
        }
