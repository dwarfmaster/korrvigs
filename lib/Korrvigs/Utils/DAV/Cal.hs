module Korrvigs.Utils.DAV.Cal
  ( CalDavData (..),
    calUser,
    calPwd,
    calManager,
    calServer,
    calCalendar,
    getCTag,
    getETags,
    getCalData,
    putCalData,
    deleteCalData,
  )
where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.List (singleton)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Utils.DAV.Web
import Network.HTTP.Conduit
import Network.URI
import System.FilePath
import System.FilePath.Lens (basename)
import Text.XML

data CalDavData = CalDavData
  { _calUser :: Text,
    _calPwd :: Text,
    _calManager :: Manager,
    _calServer :: Text,
    _calCalendar :: Text
  }

makeLenses ''CalDavData

toDavData :: CalDavData -> DavData
toDavData cdd = DavData (cdd ^. calUser) (cdd ^. calPwd) (cdd ^. calManager)

makeCalURL :: CalDavData -> Maybe Text -> Text
makeCalURL cdd ics = T.pack $ joinPath $ [T.unpack (cdd ^. calServer), "calendars", T.unpack (cdd ^. calUser), T.unpack (cdd ^. calCalendar)] ++ maybe [] (singleton . T.unpack . (<> ".ics")) ics

getCTag :: (MonadIO m) => CalDavData -> m (Either DavError Text)
getCTag cdd =
  propfind dav url [CalDavProp "getctag"] Depth0 >>= \case
    Left err -> pure $ Left err
    Right props -> case M.lookup uri props >>= M.lookup "getctag" . view statProps of
      Nothing -> pure $ Left $ DavError 207 "No CTAG in returned value"
      Just ctag -> pure $ Right ctag
  where
    dav = toDavData cdd
    url = makeCalURL cdd Nothing
    uri = maybe url ((<> "/") . T.pack . uriPath) $ parseURI (T.unpack url)

processICS :: (PropStat -> Maybe Text) -> (Text, PropStat) -> Maybe (Text, Text)
processICS ext (ics, metag) = (T.pack $ view basename $ T.unpack ics,) . T.dropAround (== '"') <$> ext metag

getETags :: (MonadIO m) => CalDavData -> m (Either DavError (Map Text Text))
getETags cdd =
  report dav url (CalProp "calendar-query") [DavProp "getetag"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processICS fromStat) $ M.toList props
  where
    dav = toDavData cdd
    url = makeCalURL cdd Nothing
    filtr p2n =
      singleton $
        Element (p2n $ CalProp "filter") M.empty $
          singleton $
            NodeElement $
              Element
                (p2n $ CalProp "comp-filter")
                (M.singleton "name" "VCALENDAR")
                [NodeElement $ Element (p2n $ CalProp "comp-filter") (M.singleton "name" "VEVENT") []]
    fromStat :: PropStat -> Maybe Text
    fromStat stat = M.lookup "getetag" $ stat ^. statProps

getCalData' :: (MonadIO m) => CalDavData -> [Text] -> m (Either DavError (Map Text Text))
getCalData' cdd ids =
  report dav url (CalProp "calendar-multiget") [CalProp "calendar-data"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processICS fromStat) $ M.toList props
  where
    dav = toDavData cdd
    url = makeCalURL cdd Nothing
    idURL i = case parseURI (T.unpack $ makeCalURL cdd $ Just i) of
      Just uri -> T.pack $ uriPath uri
      Nothing -> "/calendars/" <> cdd ^. calUser <> "/" <> cdd ^. calCalendar <> "/" <> i <> ".ics"
    filtr p2n = flip map ids $ \i ->
      Element (p2n $ DavProp "href") M.empty [NodeContent $ idURL i]
    fromStat :: PropStat -> Maybe Text
    fromStat stat = M.lookup "calendar-data" $ stat ^. statProps

getCalData :: (MonadIO m) => CalDavData -> [Text] -> m (Either DavError (Map Text Text))
getCalData cdd ids = do
  rs <- mapM (getCalData' cdd) $ chunksOf 15 ids
  let r = sequence rs
  case r of
    Left err -> pure $ Left err
    Right mps -> pure $ Right $ foldr M.union M.empty mps

-- Returns the new ETag after upload
putCalData :: (MonadIO m) => CalDavData -> Text -> Maybe Text -> LBS.ByteString -> m (Either DavError Text)
putCalData cdd i etag content =
  liftIO (put (toDavData cdd) (makeCalURL cdd $ Just i) etag content) >>= \case
    Left err -> pure $ Left err
    Right () ->
      getETags cdd <&> \case
        Left err -> Left err
        Right etags -> case M.lookup i etags of
          Nothing -> Left $ DavError 207 "Couldn't find new ETag"
          Just etg -> Right etg

-- Delete an entry
deleteCalData :: (MonadIO m) => CalDavData -> Text -> Text -> m (Either DavError ())
deleteCalData cdd i etag =
  let url = makeCalURL cdd $ Just i in liftIO $ delete (toDavData cdd) url etag
