module Korrvigs.Utils.DAV.Cal
  ( CalDavData (..),
    calUser,
    calPwd,
    calManager,
    calServer,
    calCalendar,
    getCTag,
  )
where

import Control.Lens
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Utils.DAV.Web
import Network.HTTP.Conduit
import Network.URI
import System.FilePath

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
