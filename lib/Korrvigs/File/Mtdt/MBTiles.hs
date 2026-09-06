module Korrvigs.File.Mtdt.MBTiles where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Aeson
import Data.MBTiles
import Data.Maybe
import qualified Korrvigs.Entry.JSON as Gen
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
          [ genData . Gen.ejsTitle %~ Just . fromMaybe (mtdt ^. mbName),
            maybe id (genData . Gen.ejsMetadata . at (mtdtSqlName Abstract) ?~) (toJSON <$> mtdt ^. mbDescription),
            maybe id (genData . Gen.ejsGeo ?~) (mkBounds <$> mtdt ^. mbBounds)
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
