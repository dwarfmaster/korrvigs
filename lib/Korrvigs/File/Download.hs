module Korrvigs.File.Download where

import Conduit
import Control.Applicative ((<|>))
import Control.Lens hiding (noneOf)
import Control.Monad
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.Char
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Network.HTTP.Conduit
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Status
import Network.URI
import System.FilePath
import System.IO.Temp
import Text.Parsec hiding ((<|>))

data NewDownloadedFile = NewDownloadedFile
  { _ndlUrl :: Text,
    _ndlEntry :: NewEntry
  }

makeLenses ''NewDownloadedFile

fileNameP :: Parsec ByteString () (Maybe (Bool, FilePath))
fileNameP = do
  spaces
  nm <- many1 $ satisfy (\c -> c == '*' || c == '-' || isLetter c)
  void $ char '='
  case nm of
    "filename*" -> do
      void $ string "UTF-8'"
      void $ many $ noneOf "'"
      void $ char '\''
      s <- many1 $ noneOf [';']
      let file = Enc.decodeUtf8 $ H.urlDecode True $ Enc.encodeUtf8 $ T.pack s
      pure $ Just (False, T.unpack file)
    "filename" -> Just . (True,) <$> (quoted <|> plain)
    _ -> do
      void $ many $ noneOf [';']
      pure Nothing
  where
    quoted = do
      void $ char '"'
      s <- many1 $ noneOf ['"']
      void $ char '"'
      pure s
    plain = many1 $ noneOf [';']

contDispP :: Parsec ByteString () (Maybe FilePath)
contDispP = do
  void $ many $ noneOf [';']
  option Nothing $ do
    void $ char ';'
    files <- catMaybes <$> sepBy fileNameP (char ';')
    case sort files of
      ((_, file) : _) -> pure $ Just file
      _ -> pure Nothing

contDispGetFilename :: ByteString -> Maybe FilePath
contDispGetFilename bs = case runParser contDispP () "content-disposition" bs of
  Left _ -> Nothing
  Right v -> v

newFromUrl :: (MonadKorrvigs m) => NewDownloadedFile -> m (Maybe Id)
newFromUrl dl =
  withRunInIO $ \runIO ->
    withSystemTempDirectory "korrvigsDownload" $ \dir -> do
      let url = dl ^. ndlUrl
      let urlFileName = takeFileName . uriPath <$> parseURI (T.unpack url)
      req <- parseRequest $ T.unpack $ dl ^. ndlUrl
      man <- newManager tlsManagerSettings
      success <- runResourceT $ do
        resp <- http req man
        let contentDisposition = find ((== "content-disposition") . fst) $ responseHeaders resp
        let hdFileName = contDispGetFilename . snd =<< contentDisposition
        let fileName = fromMaybe "download" $ hdFileName <|> urlFileName
        let tmp = joinPath [dir, fileName]
        let scode = statusCode (responseStatus resp)
        if scode == 200
          then runConduit (responseBody resp .| sinkFile tmp) >> pure (Just tmp)
          else pure Nothing
      case success of
        Nothing -> pure Nothing
        Just tmp -> do
          let nfile = NewFile (dl ^. ndlEntry) False & nfEntry . neMtdt %~ ((mtdtSqlName Url, toJSON url) :)
          i <- runIO $ new tmp nfile
          pure $ Just i
