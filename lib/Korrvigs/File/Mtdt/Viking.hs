module Korrvigs.File.Mtdt.Viking where

import Control.Lens
import Data.Viking
import qualified Korrvigs.Entry.JSON as Gen
import Korrvigs.File.Sync
import Korrvigs.Geometry
import Linear.V2
import Network.Mime

extract :: FilePath -> MimeType -> IO (FileMetadata -> FileMetadata)
extract path "application/x-viking" =
  parseVikingFile path >>= \case
    Left _ -> pure id
    Right vik -> do
      let geopath = concatMap segmentToPath $ vik ^.. vikTopLayers . each . vikLayers . each . vikLayerTracks . each . vikSegments . each
      pure $ genData . Gen.ejsGeo ?~ GeoPath geopath
  where
    segmentToPath :: [VikingTrackPoint] -> [Point]
    segmentToPath = fmap $ \vkpt -> V2 (vkpt ^. vikTPLon) (vkpt ^. vikTPLat)
extract _ _ = pure id
