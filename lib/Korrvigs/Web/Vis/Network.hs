module Korrvigs.Web.Vis.Network
  ( network,
    NodeShape (..),
    NodeStyle (..),
    nodeBackground,
    nodeBorder,
    nodeSelected,
    nodeHover,
    nodeFontColor,
    nodeOpacity,
    nodeShape,
    nodeMass,
    nodeBorderWidth,
    nodeLink,
    defNodeStyle,
    EdgeStyle (..),
    edgeColor,
    edgeDirected,
    defEdgeStyle,
  )
where

import Control.Lens hiding (from, to, (.=))
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Utils.Base16
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Text.Julius
import Yesod

data NodeShape
  = ShapeEllipse
  | ShapeCircle
  | ShapeBox
  | ShapeText
  | ShapeDot Int
  | ShapeStar Int
  | ShapeTriangle Int
  | ShapeTriangleDown Int
  | ShapeHexagon Int
  | ShapeSquare Int
  | ShapeImage Int Text
  | ShapeCircularImage Int Text

displayShape :: NodeShape -> Text
displayShape ShapeEllipse = "ellipse"
displayShape ShapeCircle = "circle"
displayShape ShapeBox = "box"
displayShape ShapeText = "text"
displayShape (ShapeDot _) = "dot"
displayShape (ShapeStar _) = "star"
displayShape (ShapeTriangle _) = "triangle"
displayShape (ShapeTriangleDown _) = "triangleDown"
displayShape (ShapeHexagon _) = "hexagon"
displayShape (ShapeSquare _) = "square"
displayShape (ShapeImage _ _) = "image"
displayShape (ShapeCircularImage _ _) = "circularImage"

data NodeStyle = NodeStyle
  { _nodeBackground :: Text,
    _nodeBorder :: Text,
    _nodeSelected :: Text,
    _nodeHover :: Text,
    _nodeFontColor :: Text,
    _nodeLabelInside :: Bool,
    _nodeOpacity :: Double,
    _nodeShape :: NodeShape,
    _nodeMass :: Int,
    _nodeBorderWidth :: Int,
    _nodeLink :: Maybe Text
  }

makeLenses ''NodeStyle

defNodeStyle :: Handler NodeStyle
defNodeStyle = do
  base <- getBase
  pure $
    NodeStyle
      { _nodeBackground = base Base0D,
        _nodeBorder = base Base00,
        _nodeSelected = base Base02,
        _nodeFontColor = base Base05,
        _nodeHover = base Base01,
        _nodeLabelInside = False,
        _nodeOpacity = 1.0,
        _nodeShape = ShapeDot 10,
        _nodeMass = 1,
        _nodeBorderWidth = 2,
        _nodeLink = Nothing
      }

data EdgeStyle = EdgeStyle
  { _edgeColor :: Text,
    _edgeDirected :: Bool
  }

makeLenses ''EdgeStyle

defEdgeStyle :: Handler EdgeStyle
defEdgeStyle = do
  base <- getBase
  pure $
    EdgeStyle
      { _edgeColor = base Base0D,
        _edgeDirected = True
      }

mkNodeJS :: Int -> Text -> NodeStyle -> Value
mkNodeJS i content style =
  object $
    [ "id" .= i,
      (if style ^. nodeLabelInside then "label" else "title") .= content,
      "boderWidth" .= (style ^. nodeBorderWidth),
      "color"
        .= object
          [ "border" .= (style ^. nodeBorder),
            "background" .= (style ^. nodeBackground),
            "highlight"
              .= object
                [ "background" .= (style ^. nodeBackground),
                  "border" .= (style ^. nodeSelected)
                ],
            "hover"
              .= object
                [ "background" .= (style ^. nodeBackground),
                  "border" .= (style ^. nodeHover)
                ]
          ],
      "opacity" .= (style ^. nodeOpacity),
      "font" .= object ["color" .= (style ^. nodeFontColor)],
      "mass" .= (style ^. nodeMass),
      "shape" .= displayShape (style ^. nodeShape)
    ]
      ++ maybe [] (\url -> ["image" .= url]) (getImageUrl $ style ^. nodeShape)
      ++ maybe [] (\sz -> ["size" .= sz]) (getShapeSize $ style ^. nodeShape)
  where
    getImageUrl :: NodeShape -> Maybe Text
    getImageUrl (ShapeImage _ url) = Just url
    getImageUrl (ShapeCircularImage _ url) = Just url
    getImageUrl _ = Nothing
    getShapeSize :: NodeShape -> Maybe Int
    getShapeSize (ShapeDot s) = Just s
    getShapeSize (ShapeStar s) = Just s
    getShapeSize (ShapeTriangle s) = Just s
    getShapeSize (ShapeTriangleDown s) = Just s
    getShapeSize (ShapeHexagon s) = Just s
    getShapeSize (ShapeSquare s) = Just s
    getShapeSize (ShapeImage s _) = Just s
    getShapeSize (ShapeCircularImage s _) = Just s
    getShapeSize _ = Nothing

mkEdgeJS :: Maybe Int -> Maybe Int -> EdgeStyle -> Maybe Value
mkEdgeJS (Just from) (Just to) style =
  Just $
    object
      [ "from" .= from,
        "to" .= to,
        "arrows" .= object ["to" .= object ["enabled" .= (style ^. edgeDirected)]],
        "color" .= (style ^. edgeColor)
      ]
mkEdgeJS _ _ _ = Nothing

mkLnk :: Int -> Maybe Text -> Maybe Value
mkLnk _ Nothing = Nothing
mkLnk i (Just dest) = Just $ array [toJSON i, toJSON dest]

network :: (Ord a) => Text -> [(a, Text, NodeStyle)] -> [(a, a, EdgeStyle)] -> Handler Widget
network var nodes edges = do
  let nodesWithId = zip nodes ([1 ..] :: [Int])
  let idMap = M.fromList $ (\((x, _, _), i) -> (x, i)) <$> nodesWithId
  let nodesJS = array $ map (\((_, content, style), i) -> mkNodeJS i content style) nodesWithId
  let edgesJS = array $ mapMaybe (\(from, to, style) -> mkEdgeJS (M.lookup from idMap) (M.lookup to idMap) style) edges
  let lnkJS = array $ mapMaybe (\((_, _, style), i) -> mkLnk i $ style ^. nodeLink) nodesWithId
  netId <- newIdent
  let setup = rawJS $ "setup" <> var
  pure $ do
    Rcs.visNetwork StaticR
    [whamlet|<div ##{netId}>|]
    toWidget
      [cassius|
      ##{netId}
        width: 100%
        height: 30em
    |]
    toWidget
      [julius|
      const #{setup} = function() {
        var nodes = new vis.DataSet(#{rawJSON nodesJS})
        var edges = new vis.DataSet(#{rawJSON edgesJS})
        var container = document.getElementById(#{netId})
        var data = {
          nodes: nodes,
          edges: edges
        }
        var options = {}
        var lnkMap = new Map(#{rawJSON lnkJS})
        var network = new vis.Network(container, data, options)
        network.on('doubleClick', function(e) {
          for(let node of e.nodes) {
            var dest = lnkMap.get(node)
            if (dest) {
              window.location = dest
            }
          }
        })
        return network
      }
      var #{rawJS var} = #{setup}()
    |]
