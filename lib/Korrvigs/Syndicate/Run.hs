{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Syndicate.Run where

import Conduit
import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.UTF8 as BSU8
import Data.Conduit.Aeson
import Data.Foldable1 (last)
import Data.List hiding (last)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Monoid
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Data.XML.Types
import Korrvigs.Compute
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import Korrvigs.Monad.Sync
import Korrvigs.Syndicate.Item
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.New (lazyUpdateDate, lookupFromUrl)
import Korrvigs.Syndicate.Sync (updateImpl)
import Korrvigs.Utils
import Korrvigs.Utils.JSON (sqlJsonToBool)
import Korrvigs.Utils.Time (measureTimeMs)
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import Network.HTTP.Types.URI
import Network.URI
import Opaleye hiding (not)
import System.IO
import System.Process
import qualified Text.Atom.Feed as Atom
import Text.Feed.Import
import Text.Feed.Types
import Text.HTML.TagSoup
import qualified Text.RSS.Syntax as RSS
import qualified Text.RSS1.Syntax as RSS1
import Prelude hiding (last)

data SyndicateSettingsImpl a b = SyndicateSettings
  { _runJavascript :: a,
    _keepFragment :: b
  }

type SyndicateSettings = SyndicateSettingsImpl Bool Bool

type SyndicateSettingsSQL = SyndicateSettingsImpl (Field SqlBool) (Field SqlBool)

defSettings :: SyndicateSettings
defSettings = SyndicateSettings False False

makeLenses ''SyndicateSettingsImpl
makeAdaptorAndInstanceInferrable "pSyndicateSettings" ''SyndicateSettingsImpl

-- Return Nothing if the ressource has expired or if it hasn't changed
lazyDownload ::
  (MonadIO m, MonadThrow m, MonadResource m) =>
  Manager ->
  Text ->
  Maybe UTCTime ->
  Maybe Text ->
  SyndicateSettings ->
  m (Maybe (ConduitT () BSU8.ByteString m (), Maybe Text, m ()))
lazyDownload man url expiration etag settings = runMaybeT $ do
  let runJs = settings ^. runJavascript
  forM_ expiration $ \expi -> do
    current <- liftIO getCurrentTime
    guard $ current > expi
  if not runJs
    then do
      req' <- parseRequest $ T.unpack url
      let userAgent = "Mozilla/5.0 (X11; Linux x86_64; rv:141.0) Gecko/20100101 Firefox/141.0"
      let headers = ("User-Agent", userAgent) : requestHeaders req'
      let req = req' {requestHeaders = maybe [] (\etg -> [("If-None-Match", Enc.encodeUtf8 etg)]) etag ++ headers}
      resp <- lift $ http req man
      let scode = statusCode $ responseStatus resp
      guard $ scode /= 304
      when (scode /= 200) $ throwM $ KMiscError $ "Error " <> T.pack (show scode) <> " when downloading " <> url
      let netag = lookup "ETag" $ responseHeaders resp
      pure (responseBody resp, Enc.decodeUtf8 <$> netag, pure ())
    else liftIO $ do
      let prc' = proc "chromium" ["--headless", "--disable-gpu", "--dump-dom", T.unpack url]
      devNull <- openFile "/dev/null" WriteMode
      let prc = prc' {std_out = CreatePipe, std_err = UseHandle devNull}
      (_, Just out, _, handle) <- createProcess prc
      let cleanup = liftIO $ waitForProcess handle >> hClose devNull
      pure (sourceHandle out, Nothing, cleanup)

run :: (MonadKorrvigs m, MonadResource m) => Syndicate -> m Bool
run syn = case syn ^. synUrl of
  Nothing -> pure False
  Just url -> do
    (time, (f, r)) <- measureTimeMs $ do
      man <- manager
      let sqlI = sqlInt4 $ syn ^. synEntry . entryId
      let getSqlBool = fromNullable (sqlBool False) . sqlJsonToBool
      settings <-
        fmap (fromMaybe defSettings) $
          rSelectOne $
            SyndicateSettings . getSqlBool
              <$> selectMtdt RunJavascript sqlI
              <*> (getSqlBool <$> selectMtdt KeepFragment sqlI)
      lazyDownload man url (syn ^. synExpiration) (syn ^. synETag) settings >>= \case
        Nothing -> pure (pure, False)
        Just (dat, netag, cleanup) -> do
          (imp, items) <- case syn ^. synFilters of
            [] -> runWithoutFilter settings dat
            (flt : flts) -> runWithFilters settings dat (flt :| flts)
          cleanup
          let setETag = synjsETag .~ netag
          let updItems synjs = do
                nitems <- mergeItemsInto items $ synjs ^. synjsItems
                pure $ synjs & synjsItems .~ nitems
          pure (updItems . setETag . imp, True)
    date <- liftIO getCurrentTime
    let setTime = synjsMetadata . at (mtdtSqlName RunTime) ?~ toJSON time
    let setDate = synjsMetadata . at (mtdtSqlName RunDate) ?~ toJSON (iso8601Show date)
    updateImpl syn $ f . setTime . setDate
    syncFileOfKind (syn ^. synPath) Syndicate
    pure r

sinkFeed :: (MonadKorrvigs m) => SyndicateSettings -> ConduitT BSU8.ByteString Void m (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
sinkFeed settings = sinkLazy >>= parseFeed
  where
    parseFeed lbs = lift $ do
      feed <- throwMaybe (KMiscError "Failed to parse feed") $ parseFeedSource lbs
      current <- liftIO getCurrentTime
      pure $ case feed of
        AtomFeed fd -> importFromAtom settings fd
        RSSFeed fd -> importFromRSS settings current fd
        RSS1Feed fd -> importFromRSS1 settings fd
        XMLFeed xml -> importFromXML settings xml

runWithFilters ::
  (MonadKorrvigs m, MonadResource m) =>
  SyndicateSettings ->
  ConduitT () BSU8.ByteString m () ->
  NonEmpty (Id, Text) ->
  m (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
runWithFilters settings dat flts = fromMaybeT (id, []) $ do
  comps <- forM flts $ hoistLift . uncurry getComputation
  let lastComp = last comps
  let seen = S.fromList $ (view cmpEntry &&& view cmpName) <$> NE.toList comps
  let rec i tmp arg = runIdentityT $ resolveArg i (runVeryLazy' seen) tmp arg
  let sink = case lastComp ^. cmpRun . runType of
        ArbitraryText -> sinkFeed settings
        _ -> conduitArray .| (id,) <$> sinkList
  hoistEitherLift $
    runPipe
      ((view cmpEntry &&& view cmpRun) <$> comps)
      rec
      dat
      sink

runWithoutFilter ::
  (MonadKorrvigs m, MonadResource m) =>
  SyndicateSettings ->
  ConduitT () BSU8.ByteString m () ->
  m (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
runWithoutFilter settings dat = runConduit $ dat .| sinkFeed settings

mergeItemsInto :: (MonadKorrvigs m) => [SyndicatedItem] -> [SyndicatedItem] -> m [SyndicatedItem]
mergeItemsInto = flip (foldM (flip insertOneItem)) . reverse

insertOneItem :: (MonadKorrvigs m) => SyndicatedItem -> [SyndicatedItem] -> m [SyndicatedItem]
insertOneItem it = findAndInsert
  where
    merge :: (MonadKorrvigs m) => SyndicatedItem -> SyndicatedItem -> m SyndicatedItem
    merge new old =
      tryInstantiate $
        new
          & synitInstance .~ old ^. synitInstance
          & synitRead .~ old ^. synitRead
    findAndInsert :: (MonadKorrvigs m) => [SyndicatedItem] -> m [SyndicatedItem]
    findAndInsert [] = (: []) <$> tryInstantiate it
    findAndInsert (oit : oits) | isSame it oit = (: oits) <$> merge it oit
    findAndInsert (oit : oits) = (oit :) <$> findAndInsert oits

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
normalizeURL :: SyndicateSettings -> Text -> Text
normalizeURL settings url = case parseURI $ T.unpack url of
  Nothing -> url
  Just uri ->
    let q = parseQuery $ BS8.pack $ uriQuery uri
     in let nq = filter (not . BS.isPrefixOf "utm_" . fst) q
         in T.pack $ show $ uri {uriQuery = BSU8.toString $ renderQuery True nq, uriFragment = if settings ^. keepFragment then uriFragment uri else ""}

importFromAtom :: SyndicateSettings -> Atom.Feed -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromAtom settings feed = (setTitle . setAuthors, mapMaybe importFromEntry $ Atom.feedEntries feed)
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
            _synitUrl = normalizeURL settings url,
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
extractText (Atom.XHTMLString xml) = extractXMLText xml

extractXMLText :: Element -> Text
extractXMLText = T.intercalate " " . exElem
  where
    exNode (NodeContent (ContentText txt)) = [T.strip txt]
    exNode (NodeElement el) = exElem el
    exNode _ = []
    exElem el = exNode =<< elementNodes el

importFromRSS :: SyndicateSettings -> UTCTime -> RSS.RSS -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromRSS settings time feed = (setTitle . setDesc . setTTL, mapMaybe importFromItem $ RSS.rssItems channel)
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
            _synitUrl = normalizeURL settings url,
            _synitRead = False,
            _synitGUID = RSS.rssGuidValue <$> RSS.rssItemGuid item,
            _synitDate = parseDate =<< RSS.rssItemPubDate item,
            _synitInstance = Nothing
          }

importFromRSS1 :: SyndicateSettings -> RSS1.Feed -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromRSS1 settings feed = (setTitle . setDesc, importFromItem <$> RSS1.feedItems feed)
  where
    channel = RSS1.feedChannel feed
    setTitle = synjsTitle %~ Just . fromMaybe (RSS1.channelTitle channel)
    setDesc = synjsMetadata . at (mtdtSqlName Abstract) ?~ toJSON (RSS1.channelDesc channel)
    importFromItem :: RSS1.Item -> SyndicatedItem
    importFromItem item =
      SyndicatedItem
        { _synitTitle = RSS1.itemTitle item,
          _synitUrl = normalizeURL settings $ RSS1.itemURI item,
          _synitRead = False,
          _synitGUID = Nothing,
          _synitDate = Nothing,
          _synitInstance = Nothing
        }

importFromXML :: SyndicateSettings -> Element -> (SyndicateJSON -> SyndicateJSON, [SyndicatedItem])
importFromXML settings xml = (appEndo synEndo, mapMaybe extractItem $ elementNodes xml)
  where
    checkName :: Text -> Name -> Bool
    checkName nm e =
      isNothing (namePrefix e)
        && nameLocalName e == nm
    extractTitle :: Node -> Endo SyndicateJSON
    extractTitle (NodeElement e)
      | checkName "title" (elementName e) =
          Endo $ synjsTitle %~ Just . fromMaybe (extractXMLText e)
    extractTitle _ = mempty
    synEndo :: Endo SyndicateJSON
    synEndo = foldMap extractTitle $ elementNodes xml
    extractItem :: Node -> Maybe SyndicatedItem
    extractItem (NodeElement e) = do
      guard $ checkName "entry" $ elementName e
      let f = foldMap extractItemV $ elementNodes e
      let defItem =
            SyndicatedItem
              { _synitTitle = "",
                _synitUrl = "",
                _synitRead = False,
                _synitGUID = Nothing,
                _synitDate = Nothing,
                _synitInstance = Nothing
              }
      let item = appEndo f defItem
      guard $ not $ T.null $ item ^. synitTitle
      guard $ not $ T.null $ item ^. synitUrl
      pure item
    extractItem _ = Nothing
    extractItemV :: Node -> Endo SyndicatedItem
    extractItemV (NodeElement e)
      | checkName "title" (elementName e) =
          Endo $ synitTitle .~ extractXMLText e
    extractItemV (NodeElement e)
      | checkName "link" (elementName e) =
          maybe mempty (Endo . (synitUrl .~) . normalizeURL settings . extractContentsText . snd) $ find (checkName "href" . fst) $ elementAttributes e
    extractItemV (NodeElement e)
      | checkName "id" (elementName e) =
          Endo $ synitGUID ?~ extractXMLText e
    extractItemV (NodeElement e)
      | checkName "published" (elementName e) =
          maybe mempty (Endo . (synitDate ?~) . zonedTimeToUTC) $ iso8601ParseM $ T.unpack $ extractXMLText e
    extractItemV _ = mempty
    extractContentsText :: [Content] -> Text
    extractContentsText cnts = T.intercalate " " $ extractContentText =<< cnts
    extractContentText :: Content -> [Text]
    extractContentText (ContentText txt) = [T.strip txt]
    extractContentText _ = []
