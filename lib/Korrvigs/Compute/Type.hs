module Korrvigs.Compute.Type where

import Control.Arrow ((&&&))
import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens
import Data.Base64.Types
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Base64 as B64
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LEnc

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right v) = Just v
rightToMaybe _ = Nothing

data RunnableType
  = RunError
  | ScalarImage
  | ScalarGraphic
  | VectorGraphic
  | ArbitraryJson
  | ArbitraryText
  | TabularCsv
  | Model3D
  deriving (Show, Eq, Ord, Enum, Bounded)

data RunnableKind = KindErr | KindText | KindBin | KindJson
  deriving (Show, Eq, Ord, Enum, Bounded)

data RunnableResult
  = ResultError Text
  | ResultText Text
  | ResultBinary ByteString
  | ResultJson Value
  deriving (Show, Eq)

makePrisms ''RunnableResult

extractResult :: RunnableType -> ByteString -> Maybe RunnableResult
extractResult tp stream = case runTypeKind tp of
  KindErr -> toTxt ResultError
  KindText -> toTxt ResultText
  KindBin -> Just $ ResultBinary stream
  KindJson -> ResultJson <$> rightToMaybe (eitherDecode stream)
  where
    toTxt cstr = cstr . LT.toStrict <$> rightToMaybe (LEnc.decodeUtf8' stream)

runTypeKind :: RunnableType -> RunnableKind
runTypeKind RunError = KindErr
runTypeKind ScalarImage = KindBin
runTypeKind ScalarGraphic = KindBin
runTypeKind VectorGraphic = KindText
runTypeKind ArbitraryJson = KindJson
runTypeKind ArbitraryText = KindText
runTypeKind TabularCsv = KindText
runTypeKind Model3D = KindBin

runTypeName :: RunnableType -> Text
runTypeName RunError = "error"
runTypeName ScalarImage = "image"
runTypeName ScalarGraphic = "graphic"
runTypeName VectorGraphic = "vector"
runTypeName ArbitraryJson = "json"
runTypeName ArbitraryText = "text"
runTypeName TabularCsv = "csv"
runTypeName Model3D = "3d"

runTypeExt :: RunnableType -> Text
runTypeExt RunError = "log"
runTypeExt ScalarImage = "jpg"
runTypeExt ScalarGraphic = "png"
runTypeExt VectorGraphic = "svg"
runTypeExt ArbitraryJson = "json"
runTypeExt ArbitraryText = "txt"
runTypeExt TabularCsv = "csv"
runTypeExt Model3D = "glb"

parseTypeName :: Text -> Maybe RunnableType
parseTypeName =
  flip M.lookup $ M.fromList $ (runTypeName &&& id) <$> [minBound .. maxBound]

encodeToJSON :: RunnableResult -> Value
encodeToJSON (ResultError err) = toJSON err
encodeToJSON (ResultText txt) = toJSON txt
encodeToJSON (ResultBinary bin) = toJSON $ extractBase64 $ B64.encodeBase64 bin
encodeToJSON (ResultJson v) = v

encodeToText :: RunnableResult -> Text
encodeToText (ResultError err) = err
encodeToText (ResultText txt) = txt
encodeToText (ResultBinary bin) =
  LT.toStrict $
    LT.intercalate "\n" $
      LT.chunksOf 110 $
        extractBase64 $
          B64.encodeBase64 bin
encodeToText (ResultJson v) = LT.toStrict $ LEnc.decodeUtf8 $ encodePretty v

encodeToLBS :: RunnableResult -> LBS.ByteString
encodeToLBS (ResultError err) = LEnc.encodeUtf8 $ LT.fromStrict err
encodeToLBS (ResultText txt) = LEnc.encodeUtf8 $ LT.fromStrict txt
encodeToLBS (ResultBinary bin) = bin
encodeToLBS (ResultJson v) = encode v

decodeTextFromJson :: Value -> Maybe Text
decodeTextFromJson v = v ^? _String

decodeBinFromJson :: Value -> Maybe ByteString
decodeBinFromJson v =
  v ^? _String . to (B64.decodeBase64Untyped . LEnc.encodeUtf8 . LT.fromStrict) . _Right

decodeJsonFromJson :: Value -> Maybe Value
decodeJsonFromJson = Just

decodeFromJson :: RunnableType -> Value -> Maybe RunnableResult
decodeFromJson tp val = case runTypeKind tp of
  KindErr -> ResultError <$> decodeTextFromJson val
  KindText -> ResultText <$> decodeTextFromJson val
  KindBin -> ResultBinary <$> decodeBinFromJson val
  KindJson -> ResultJson <$> decodeJsonFromJson val

decodeTextFromText :: Text -> Maybe Text
decodeTextFromText = Just

decodeBinFromText :: Text -> Maybe ByteString
decodeBinFromText =
  rightToMaybe
    . B64.decodeBase64Untyped
    . LEnc.encodeUtf8
    . LT.fromStrict
    . T.filter (`notElem` ['\n', ' '])

decodeJsonFromText :: Text -> Maybe Value
decodeJsonFromText = rightToMaybe . eitherDecode . LEnc.encodeUtf8 . LT.fromStrict

decodeFromText :: RunnableType -> Text -> Maybe RunnableResult
decodeFromText tp txt = case runTypeKind tp of
  KindErr -> ResultError <$> decodeTextFromText txt
  KindText -> ResultText <$> decodeTextFromText txt
  KindBin -> ResultBinary <$> decodeBinFromText txt
  KindJson -> ResultJson <$> decodeJsonFromText txt
