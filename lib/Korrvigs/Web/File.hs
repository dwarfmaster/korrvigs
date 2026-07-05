module Korrvigs.Web.File (getFileRasterTileTopR, getFileRasterTileR) where

import Control.Lens hiding ((.=))
import Control.Monad.Trans.Resource
import qualified Data.Binary.Builder as Bld
import qualified Data.ByteString as BS
import Data.List (singleton)
import Data.MBTiles
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad hiding (loadMetadata)
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Yesod

getFileRasterTileTopR :: WebId -> Handler Value
getFileRasterTileTopR (WId i) = do
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load file")
  file <- throwMaybe (KMiscError $ unId i <> " is not a file") $ entry ^? _File
  runResourceT $ do
    mb <- openMBFile $ file ^. filePath
    mtdt <- liftIO $ loadMetadata mb
    pure $
      object $
        mconcat
          [ maybe [] (singleton . ("name" .=)) (mtdt ^? _Just . mbName),
            maybe [] (singleton . ("format" .=) . printFormat) (mtdt ^? _Just . mbFormat),
            maybe [] (singleton . ("minzoom" .=)) (mtdt ^? _Just . mbMinZoom . _Just),
            maybe [] (singleton . ("maxzoom" .=)) (mtdt ^? _Just . mbMaxZoom . _Just)
          ]
  where
    printFormat :: MBFormat -> Text
    printFormat MBPng = "png"
    printFormat MBJpg = "jpg"
    printFormat MBPbf = "pbf"

getFileRasterTileR :: WebId -> Int -> Int -> Int -> Handler TypedContent
getFileRasterTileR (WId i) zm lat lon = do
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load file")
  file <- throwMaybe (KMiscError $ unId i <> " is not a file") $ entry ^? _File
  runResourceT $ do
    mb <- openMBFile $ file ^. filePath
    mtdt <- liftIO $ loadMetadata mb
    let mime = case mtdt ^? _Just . mbFormat of
          Just MBPng -> typePng
          Just MBJpg -> typeJpeg
          _ -> typePlain
    let actualLat = ((2 :: Int) ^ zm) - 1 - lat
    tile <- liftIO $ loadTile mb lon actualLat zm
    case tile of
      Nothing -> lift notFound
      Just tileDat -> pure $ toTypedContent (mime, ContentBuilder (Bld.fromByteString tileDat) (Just $ BS.length tileDat))
