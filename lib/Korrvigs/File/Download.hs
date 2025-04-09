module Korrvigs.File.Download where

import Conduit
import Control.Lens
import Data.Aeson (toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Network.HTTP.Conduit
import Network.HTTP.Types.Status
import System.IO
import System.IO.Temp

data NewDownloadedFile = NewDownloadedFile
  { _ndlUrl :: Text,
    _ndlEntry :: NewEntry
  }

makeLenses ''NewDownloadedFile

newFromUrl :: (MonadKorrvigs m) => NewDownloadedFile -> m (Maybe Id)
newFromUrl dl =
  withRunInIO $ \runIO ->
    withSystemTempFile "korrvigsDownload" $ \tmp handle -> do
      hClose handle
      let url = dl ^. ndlUrl
      req <- parseRequest $ T.unpack $ dl ^. ndlUrl
      man <- newManager tlsManagerSettings
      success <- runResourceT $ do
        resp <- http req man
        let scode = statusCode (responseStatus resp)
        if scode == 200
          then runConduit (responseBody resp .| sinkFile tmp) >> pure True
          else pure False
      if not success
        then pure Nothing
        else do
          let nfile = NewFile (dl ^. ndlEntry) & nfEntry . neMtdt %~ ((mtdtSqlName Url, toJSON url) :)
          i <- runIO $ new tmp nfile
          pure $ Just i
