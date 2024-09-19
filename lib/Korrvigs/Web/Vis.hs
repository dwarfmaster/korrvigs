module Korrvigs.Web.Vis
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
    defNodeStyle,
    EdgeStyle (..),
    edgeColor,
    edgeDirected,
    defEdgeStyle,
  )
where

import Control.Lens hiding (from, to, (.=))
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Data.Text.Lazy.Builder
import Korrvigs.Utils.Base16
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Text.Blaze.Html.Renderer.Text
import Text.Julius
import Yesod

buildHtml :: Html -> Builder
buildHtml = fromLazyText . renderHtml

data NodeShape
  = ShapeEllipse
  | ShapeCircle
  | ShapeBox
  | ShapeText

displayShape :: NodeShape -> Text
displayShape ShapeEllipse = "ellipse"
displayShape ShapeCircle = "circle"
displayShape ShapeBox = "box"
displayShape ShapeText = "text"

data NodeStyle = NodeStyle
  { _nodeBackground :: Text,
    _nodeBorder :: Text,
    _nodeSelected :: Text,
    _nodeHover :: Text,
    _nodeFontColor :: Text,
    _nodeOpacity :: Double,
    _nodeShape :: NodeShape,
    _nodeMass :: Int,
    _nodeBorderWidth :: Int
  }

makeLenses ''NodeStyle

defNodeStyle :: Handler NodeStyle
defNodeStyle = do
  base <- getBase
  pure $
    NodeStyle
      { _nodeBackground = base Base00,
        _nodeBorder = base Base0D,
        _nodeSelected = base Base02,
        _nodeFontColor = base Base05,
        _nodeHover = base Base01,
        _nodeOpacity = 1.0,
        _nodeShape = ShapeEllipse,
        _nodeMass = 1,
        _nodeBorderWidth = 2
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

rawJSON :: Value -> RawJavascript
rawJSON = rawJS . encodeToTextBuilder

mkNodeJS :: Int -> Html -> NodeStyle -> Value
mkNodeJS i content style =
  object
    [ "id" .= i,
      "label" .= toLazyText (buildHtml content),
      "boderWidth" .= (style ^. nodeBorderWidth),
      "color"
        .= object
          [ "border" .= (style ^. nodeBorder),
            "background" .= (style ^. nodeBackground),
            "highlight" .= object ["background" .= (style ^. nodeSelected)],
            "hover" .= object ["background" .= (style ^. nodeHover)]
          ],
      "opacity" .= (style ^. nodeOpacity),
      "font" .= object ["color" .= (style ^. nodeFontColor)],
      "mass" .= (style ^. nodeMass),
      "shape" .= displayShape (style ^. nodeShape)
    ]

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

network :: (Ord a) => Text -> [(a, Html, NodeStyle)] -> [(a, a, EdgeStyle)] -> Handler Widget
network var nodes edges = do
  let nodesWithId = zip nodes ([1 ..] :: [Int])
  let idMap = M.fromList $ (\((x, _, _), i) -> (x, i)) <$> nodesWithId
  let nodesJS = array $ map (\((_, content, style), i) -> mkNodeJS i content style) nodesWithId
  let edgesJS = array $ mapMaybe (\(from, to, style) -> mkEdgeJS (M.lookup from idMap) (M.lookup to idMap) style) edges
  netId <- newIdent
  let setup = rawJS $ "setup" <> var
  pure $ do
    Rcs.vis
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
        return new vis.Network(container, data, options)
      }
      var #{rawJS var} = #{setup}()
    |]
