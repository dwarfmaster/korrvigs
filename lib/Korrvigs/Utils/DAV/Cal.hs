module Korrvigs.Utils.DAV.Cal
  ( CalDavData (..),
    calUser,
    calPwd,
    calManager,
    calServer,
    calCalendar,
    getCTag,
    getETags,
  )
where

import Control.Arrow (second)
import Control.Lens
import Control.Monad.IO.Class
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

makeCalURL :: CalDavData -> Text
makeCalURL cdd = T.pack $ joinPath [T.unpack (cdd ^. calServer), "calendars", T.unpack (cdd ^. calUser), T.unpack (cdd ^. calCalendar)]

getCTag :: (MonadIO m) => CalDavData -> m (Either DavError Text)
getCTag cdd =
  propfind dav url [CalDavProp "getctag"] Depth0 >>= \case
    Left err -> pure $ Left err
    Right props -> case M.lookup uri props >>= M.lookup "getctag" . view statProps of
      Nothing -> pure $ Left $ DavError 207 "No CTAG in returned value"
      Just ctag -> pure $ Right ctag
  where
    dav = toDavData cdd
    url = makeCalURL cdd
    uri = maybe url ((<> "/") . T.pack . uriPath) $ parseURI (T.unpack url)

getETags :: (MonadIO m) => CalDavData -> m (Either DavError (Map Text Text))
getETags cdd =
  report dav url [DavProp "getetag"] filtr Depth1 >>= \case
    Left err -> pure $ Left err
    Right props -> pure $ Right $ M.fromList $ mapMaybe (processICS . second fromStat) $ M.toList props
  where
    dav = toDavData cdd
    url = makeCalURL cdd
    filtr p2n =
      Element
        (p2n $ CalProp "comp-filter")
        (M.singleton "name" "VCALENDAR")
        [NodeElement $ Element (p2n $ CalProp "comp-filter") (M.singleton "name" "VEVENT") []]
    fromStat :: PropStat -> Maybe Text
    fromStat stat = M.lookup "getetag" $ stat ^. statProps
    processICS :: (Text, Maybe Text) -> Maybe (Text, Text)
    processICS (_, Nothing) = Nothing
    processICS (ics, Just etag) = Just (T.pack $ view basename $ T.unpack ics, etag)
