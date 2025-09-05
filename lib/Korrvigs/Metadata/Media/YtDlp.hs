module Korrvigs.Metadata.Media.YtDlp where

import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.Trans
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Opaleye
import System.IO
import System.Process

parseQuery :: Text -> Maybe Text
parseQuery url | "https://www.youtube.com/" `T.isPrefixOf` url = Just url
parseQuery _ = Nothing

data Type = YtVideo | YtPlaylist
  deriving (Eq)

data YtMetadata = YtMetadata
  { _ytType :: Type,
    _ytTitle :: Text,
    _ytDescription :: Text,
    _ytDuration :: Maybe Int,
    _ytUploaderUrl :: Maybe Text,
    _ytThumbnails :: [(Text, Maybe Int)],
    _ytThumbnail :: Maybe Text,
    _ytUploadDate :: Maybe Day
  }

makeLenses ''YtMetadata

parseType :: Text -> Parser Type
parseType "video" = pure YtVideo
parseType "playlist" = pure YtPlaylist
parseType tp = fail $ T.unpack tp <> " is not a recognised yt-dlp type"

parseThumbnail :: Value -> Parser (Text, Maybe Int)
parseThumbnail = withObject "yt-dlp thumbnail" $ \obj ->
  (,)
    <$> obj .: "url"
    <*> obj .:? "preference"

parseDate :: Text -> Parser Day
parseDate = parseTimeM True defaultTimeLocale "%0Y%m%d" . T.unpack

instance FromJSON YtMetadata where
  parseJSON = withObject "yt-dlp metadata" $ \obj ->
    YtMetadata
      <$> (obj .: "_type" >>= parseType)
      <*> obj .: "title"
      <*> obj .: "description"
      <*> obj .:? "duration"
      <*> obj .:? "uploader_url"
      <*> (obj .: "thumbnails" >>= mapM parseThumbnail)
      <*> obj .:? "thumbnail"
      <*> (obj .:? "upload_date" >>= mapM parseDate)

queryYtDlp :: (MonadKorrvigs m) => Text -> m (Maybe (NewEntry -> NewEntry))
queryYtDlp url = do
  let cmd = proc "yt-dlp" ["--dump-single-json", "--skip-download", "--playlist-items", "0,0", T.unpack url]
  devNull <- liftIO $ openFile "/dev/null" WriteMode
  (_, out, _, p) <- liftIO $ createProcess cmd {std_out = CreatePipe, std_err = UseHandle devNull}
  content <- liftIO $ mapM LBS.hGetContents out
  case eitherDecode <$> content of
    Just (Right dat) -> do
      let tp = case dat ^. ytType of
            YtVideo -> Video
            YtPlaylist -> Channel
      author <- fmap join $ forM (dat ^. ytUploaderUrl) $ \upUrl ->
        if dat ^. ytType == YtVideo
          then rSelectOne $ do
            mtdt <- selectTable entriesMetadataTable
            where_ $ mtdt ^. sqlKey .== sqlStrictText (mtdtSqlName Url)
            where_ $ mtdt ^. sqlValue .== sqlValueJSONB upUrl
            nameFor $ mtdt ^. sqlEntry
          else pure Nothing
      void $ liftIO $ waitForProcess p
      void $ liftIO $ hClose devNull
      pure $
        Just $
          foldr
            (.)
            (setMtdtValue MediaMtdt tp)
            [ setMtdtValue Abstract $ dat ^. ytDescription,
              neTitle ?~ dat ^. ytTitle,
              maybe id (neDate ?~) $ dat ^. ytUploadDate,
              setMtdtValueM TimeEstimation $ fmap ((/ 60.0) . fromIntegral) $ dat ^. ytDuration,
              neParents %~ maybe id (:) author,
              maybe id (neCover ?~) $ thumbCandidate dat,
              setMtdtValue Url url,
              if tp == Channel then setMtdtValue HubMtdt "yes" else id
            ]
    _ -> do
      void $ liftIO $ waitForProcess p
      liftIO $ hClose devNull
      pure Nothing
  where
    thumbCandidate dat =
      listToMaybe $ maybe id (:) (dat ^. ytThumbnail) $ fst <$> sortBy orderCandidates (dat ^. ytThumbnails)
    orderCandidates (_, Nothing) (_, Nothing) = EQ
    orderCandidates (_, Nothing) (_, Just _) = LT
    orderCandidates (_, Just _) (_, Nothing) = GT
    orderCandidates (_, Just a) (_, Just b) = compare b a
