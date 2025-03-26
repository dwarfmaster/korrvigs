{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Korrvigs.Utils.DAV.Cal
  ( CalDavData (..),
    DavTag (..),
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
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
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
import Text.XML

data CalDavData = CalDavData
  { _calUser :: Text,
    _calPwd :: Text,
    _calManager :: Manager,
    _calServer :: Text,
    _calCalendar :: Text
  }

makeLenses ''CalDavData

newtype DavTag = DavTag {extractDavTag :: Text} deriving (Ord, Eq, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

toDavData :: CalDavData -> DavData
toDavData cdd = DavData (cdd ^. calUser) (cdd ^. calPwd) (cdd ^. calManager)

makeCalURL :: CalDavData -> Maybe Text -> Text
makeCalURL cdd ics = T.pack $ joinPath $ [T.unpack (cdd ^. calServer), "calendars", T.unpack (cdd ^. calUser), T.unpack (cdd ^. calCalendar)] ++ maybe [] (singleton . T.unpack . (<> ".ics")) ics

getCTag :: (MonadIO m) => CalDavData -> m (Either DavError DavTag)
getCTag cdd =
  propfind dav url [CalDavProp "getctag"] Depth0 >>= \case
    Left err -> pure $ Left err
    Right props -> case M.lookup uri props >>= M.lookup "getctag" . view statProps of
      Nothing -> pure $ Left $ DavError 207 "No CTAG in returned value"
      Just ctag -> pure $ Right $ DavTag ctag
  where
    dav = toDavData cdd
    url = makeCalURL cdd Nothing
    uri = DavRc $ maybe url ((<> "/") . T.pack . uriPath) $ parseURI (T.unpack url)

processICS :: (Text -> a) -> (PropStat -> Maybe Text) -> (DavRessource, PropStat) -> Maybe (DavRessource, a)
processICS f ext (ics, metag) = (ics,) . f <$> ext metag

getETags :: (MonadIO m) => CalDavData -> m (Either DavError (Map DavRessource DavTag))
getETags cdd =
  report dav url (CalProp "calendar-query") [DavProp "getetag"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processICS DavTag fromStat) $ M.toList props
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

getCalData' :: (MonadIO m) => CalDavData -> [DavRessource] -> m (Either DavError (Map DavRessource Text))
getCalData' cdd ids =
  report dav url (CalProp "calendar-multiget") [CalProp "calendar-data"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processICS id fromStat) $ M.toList props
  where
    dav = toDavData cdd
    url = makeCalURL cdd Nothing
    filtr p2n = flip map ids $ \i ->
      Element (p2n $ DavProp "href") M.empty [NodeContent $ extractDavRc i]
    fromStat :: PropStat -> Maybe Text
    fromStat stat = M.lookup "calendar-data" $ stat ^. statProps

getCalData :: (MonadIO m) => CalDavData -> [DavRessource] -> m (Either DavError (Map DavRessource Text))
getCalData cdd ids = do
  rs <- mapM (getCalData' cdd) $ chunksOf 15 ids
  let r = sequence rs
  case r of
    Left err -> pure $ Left err
    Right mps -> pure $ Right $ foldr M.union M.empty mps

-- Returns the new ETag after upload
putCalData :: (MonadIO m) => CalDavData -> Text -> Maybe DavTag -> LBS.ByteString -> m (Either DavError DavTag)
putCalData cdd i etag content =
  liftIO (put (toDavData cdd) (makeCalURL cdd $ Just i) (extractDavTag <$> etag) content) >>= \case
    Left err -> pure $ Left err
    Right (Just netag) -> pure $ Right $ DavTag netag
    Right Nothing -> case etag of
      Just etg -> pure $ Right etg
      Nothing -> pure $ Left $ DavError 204 "No ETag returned for new upload"

-- Delete an entry
deleteCalData :: (MonadIO m) => CalDavData -> Text -> DavTag -> m (Either DavError ())
deleteCalData cdd i etag =
  let url = makeCalURL cdd $ Just i in liftIO $ delete (toDavData cdd) url $ extractDavTag etag
