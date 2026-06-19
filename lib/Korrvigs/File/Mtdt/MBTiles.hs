module Korrvigs.File.Mtdt.MBTiles where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.MBTiles
import Data.Maybe
import Korrvigs.File.Sync
import Korrvigs.Geometry
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Linear.V2
import Network.Mime
import System.FilePath

extract :: FilePath -> MimeType -> IO (FileMetadata -> FileMetadata)
extract path _ | takeExtension path == ".mbtiles" = runResourceT $ do
  file <- openMBFile path
  liftIO (loadMetadata file) >>= \case
    Nothing -> pure id
    Just mtdt ->
      pure $
        foldr
          (.)
          id
          [ exTitle %~ Just . fromMaybe (mtdt ^. mbName),
            maybe id (annoted . at (mtdtSqlName Abstract) ?~) (toJSON <$> mtdt ^. mbDescription),
            maybe id (exGeo ?~) (mkBounds <$> mtdt ^. mbBounds)
          ]
  where
    mkBounds :: MBBounds -> Geometry
    mkBounds bounds =
      let latm = bounds ^. mbMinLat
       in let lonm = bounds ^. mbMinLon
           in let latM = bounds ^. mbMaxLat
               in let lonM = bounds ^. mbMaxLon
                   in GeoPolygon (Polygon [V2 latm lonm, V2 latm lonM, V2 latM lonM, V2 latM lonm] [])
extract _ _ = pure id
