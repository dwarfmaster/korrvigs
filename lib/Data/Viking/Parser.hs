module Data.Viking.Parser (parseViking, parseVikingFile) where

import Control.Lens hiding (noneOf)
import Control.Monad
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Viking.Data
import Text.Parsec
import Text.Parsec.Number

parseVikingFile :: FilePath -> IO (Either Text VikingFile)
parseVikingFile path = parseViking <$> LBS.readFile path

parseViking :: LBS.ByteString -> Either Text VikingFile
parseViking content = case parse fileP "<viking>" content of
  Left err -> Left $ T.pack $ show err
  Right v -> Right v

keyP :: (Stream s Identity Char) => ParsecT s () Identity Text
keyP = fmap T.pack $ many1 $ letter <|> char '_'

valueP :: (Stream s Identity Char) => ParsecT s () Identity Text
valueP = fmap T.pack $ anyChar `manyTill` newline

commentP :: (Stream s Identity Char) => ParsecT s () Identity ()
commentP =
  (void $ char '#' >> manyTill anyChar newline)
    <|> (void newline)

topCommandP :: (Stream s Identity Char) => Text -> ParsecT s () Identity Text
topCommandP cmd = do
  void $ try $ string $ '~' : T.unpack cmd
  void $ many1 $ char ' '
  kd <- keyP
  void newline
  pure kd

topLayerP :: (Stream s Identity Char) => ParsecT s () Identity VikingTopLayer
topLayerP = do
  tlKind <- topCommandP "TopLayer"
  let tl = VikingTopLayer tlKind "" M.empty []
  acts <- manyTill topLayerActP (try (string "~EndTopLayer") >> newline)
  pure $ appEndo (mconcat acts) tl

keyValueP :: (Stream s Identity Char) => ParsecT s () Identity (Text, Text)
keyValueP = do
  k <- keyP
  void $ char '='
  v <- valueP
  pure (k, v)

fileP :: (Stream s Identity Char) => ParsecT s () Identity VikingFile
fileP = do
  acts <- manyTill topP eof
  pure $ appEndo (mconcat acts) $ VikingFile M.empty []

topP :: (Stream s Identity Char) => ParsecT s () Identity (Endo VikingFile)
topP =
  (commentP *> mempty)
    <|> (topLayerP >>= \tl -> pure $ Endo $ vikTopLayers %~ (<> [tl]))
    <|> (keyValueP >>= \(k, v) -> pure $ Endo $ vikKeys . at k ?~ v)

topLayerActP :: (Stream s Identity Char) => ParsecT s () Identity (Endo VikingTopLayer)
topLayerActP =
  (commentP *> mempty)
    <|> (layerP >>= \l -> pure $ Endo $ vikLayers %~ (<> [l]))
    <|> (keyValueP >>= pure . Endo . insertKV)
  where
    insertKV ("name", v) = vikTopName .~ v
    insertKV (k, v) = vikTopKeys . at k ?~ v

layerP :: (Stream s Identity Char) => ParsecT s () Identity VikingLayer
layerP = do
  lType <- topCommandP "Layer"
  acts <- manyTill layerActP (try (string "~EndLayer") >> newline)
  pure $ appEndo (mconcat acts) $ VikingLayer lType "" M.empty [] []

layerActP :: (Stream s Identity Char) => ParsecT s () Identity (Endo VikingLayer)
layerActP =
  (commentP *> mempty)
    <|> (keyValueP >>= pure . Endo . insertKV)
    <|> layerDataP
  where
    insertKV ("name", v) = vikLayerName .~ v
    insertKV (k, v) = vikLayerKeys . at k ?~ v

layerDataP :: (Stream s Identity Char) => ParsecT s () Identity (Endo VikingLayer)
layerDataP = do
  void $ try $ string "~LayerData"
  acts <- manyTill layerDataActP (try $ string "~EndLayerData")
  pure $ mconcat acts

layerDataActP :: (Stream s Identity Char) => ParsecT s () Identity (Endo VikingLayer)
layerDataActP =
  (commentP *> mempty)
    <|> (layerWaypointsP >>= \waypts -> pure $ Endo $ vikLayerWaypoints %~ (++ waypts))
    <|> (layerTrackP >>= \track -> pure $ Endo $ vikLayerTracks %~ (++ [track]))

layerWaypointsP :: (Stream s Identity Char) => ParsecT s () Identity [VikingWayPoint]
layerWaypointsP = do
  void $ try (string "type=\"waypointlist\"") >> newline
  waypoints <- manyTill waypointP $ try (string "type=\"waypointlistend\"") >> newline
  pure $ catMaybes waypoints

pointP :: (Stream s Identity Char) => ParsecT s () Identity (Double, Double, Map Text Text)
pointP = do
  acts <- flip manyTill newline $ do
    k <- keyP
    void $ char '='
    case k of
      "latitude" -> do
        v <- between (char '"') (char '"') (floating2 True) <* optional (char ' ')
        pure $ Endo $ _1 .~ v
      "longitude" -> do
        v <- between (char '"') (char '"') (floating2 True) <* optional (char ' ')
        pure $ Endo $ _2 .~ v
      _ -> do
        v <-
          between (char '"') (char '"') (many $ noneOf "\"") <* optional (char ' ')
            <|> (char '#' >> replicateM 6 anyChar)
        pure $ Endo $ _3 . at k ?~ T.pack v
  pure $ appEndo (mconcat acts) (0.0, 0.0, M.empty)

waypointP :: (Stream s Identity Char) => ParsecT s () Identity (Maybe VikingWayPoint)
waypointP = fromPoint <$> pointP
  where
    fromPoint (lat, lon, dat)
      | M.lookup "type" dat == Just "waypoint" =
          Just $
            VikingWayPoint
              { _vikWPName = fromMaybe "" $ M.lookup "name" dat,
                _vikWPLat = lat,
                _vikWPLon = lon
              }
    fromPoint _ = Nothing

layerTrackP :: (Stream s Identity Char) => ParsecT s () Identity VikingTrack
layerTrackP = do
  (nm, color) <- try $ do
    p <- pointP
    guard $ p ^. _3 . at "type" == Just "track"
    pure (p ^. _3 . at "name", p ^. _3 . at "color")
  pts <- manyTill trackpointP $ try (string "type=\"trackend\"") >> newline
  pure $
    VikingTrack
      { _vikTrackName = fromMaybe "" nm,
        _vikTrackColor = fromMaybe "000000" color,
        _vikSegments = segment (catMaybes pts) []
      }

segment :: [(Bool, VikingTrackPoint)] -> [[VikingTrackPoint]] -> [[VikingTrackPoint]]
segment [] acc = reverse $ reverse <$> acc
segment ((True, pt) : pts) acc = segment pts $ [pt] : acc
segment ((False, pt) : pts) [] = segment pts [[pt]]
segment ((False, pt) : pts) acc = segment pts $ acc & _head %~ (pt :)

trackpointP :: (Stream s Identity Char) => ParsecT s () Identity (Maybe (Bool, VikingTrackPoint))
trackpointP = fromPoint <$> pointP
  where
    fromPoint (lat, lon, dat)
      | M.lookup "type" dat == Just "trackpoint" =
          Just $
            ( M.lookup "newsegment" dat == Just "yes",
              VikingTrackPoint
                { _vikTPLat = lat,
                  _vikTPLon = lon
                }
            )
    fromPoint _ = Nothing
