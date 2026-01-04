module Korrvigs.File.Computation (fileComputations, hasModel) where

import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.Type
import Korrvigs.Entry
import NeatInterpolation
import Network.Mime

fileComputations :: Id -> MimeType -> Map Text Runnable
fileComputations i mime =
  mconcat
    [ miniature i mime,
      size i mime,
      model i mime
    ]

mkRunnable :: Text -> RunnableType -> Id -> Text -> Map Text Runnable
mkRunnable nm tp i code =
  M.singleton nm $
    Runnable
      { _runExecutable = Bash,
        _runType = tp,
        _runArgs = [ArgEntry i],
        _runEnv = M.empty,
        _runStdIn = Nothing,
        _runCode = code
      }

mkMinRunnable :: Id -> Text -> Map Text Runnable
mkMinRunnable = mkRunnable "miniature" ScalarImage

miniature :: Id -> MimeType -> Map Text Runnable
miniature i mime
  | "image/" `BS.isPrefixOf` mime || mime == "application/pdf" =
      mkMinRunnable i "magick \"$1[0]\" -resize 200x200 JPEG:-"
miniature i mime
  | "video/" `BS.isPrefixOf` mime =
      mkMinRunnable i "ffmpeg -i \"file:$1\" -vframes 1 -f image2 -vf scale=200:-2 -"
miniature _ _ = M.empty

mkSizeRunnable :: Id -> Text -> Map Text Runnable
mkSizeRunnable = mkRunnable "size" ArbitraryJson

size :: Id -> MimeType -> Map Text Runnable
size i mime
  | "image/" `BS.isPrefixOf` mime || mime == "application/pdf" =
      mkSizeRunnable
        i
        [trimming|magick identify -auto-orient -format "{\"width\": %w, \"height\": %h}\n" "$$1[0]"|]
size i mime
  | "video/" `BS.isPrefixOf` mime =
      mkSizeRunnable
        i
        [trimming|
          output=$$(ffprobe "$$1" -v error -show_entries stream=width,height -show_entries stream_side_data=rotation -print_format json | jq ".streams[0]")
          rot=$$(echo $$output | jq ".side_data_list[0].rotation")
          if [ $$rot == "90" ] || [ $$rot == "-90" ]; then
            echo "{\"width\": $$(echo $$output | jq .height), \"height\": $$(echo $$output | jq .width)}"
          else
            echo "{\"width\": $$(echo $$output | jq .width), \"height\": $$(echo $$output | jq .height)}"
          fi
        |]
size _ _ = M.empty

mkModelRunnable :: Id -> Text -> Map Text Runnable
mkModelRunnable = mkRunnable "model" Model3D

assimpMimes :: [MimeType]
assimpMimes =
  [ "model/obj",
    "model/gltf+json",
    "model/stl",
    "model/x.stl-ascii",
    "model/x.stl-binary"
  ]

hasModel :: MimeType -> Bool
hasModel = flip elem assimpMimes

model :: Id -> MimeType -> Map Text Runnable
model i mime
  | hasModel mime =
      mkModelRunnable
        i
        [trimming|
    assimp export $$1 model.glb
    cat model.glb
  |]
model _ _ = M.empty
