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
  = ScalarImage
  | ScalarGraphic
  | VectorGraphic
  | ArbitraryJson
  | ArbitraryText
  | TabularCsv
  deriving (Show, Eq, Ord, Enum, Bounded)

data RunnableKind = KindText | KindBin | KindJson
  deriving (Show, Eq, Ord, Enum, Bounded)

data RunnableResult
  = ResultText Text
  | ResultBinary ByteString
  | ResultJson Value
  deriving (Show, Eq)

extractResult :: RunnableType -> ByteString -> Maybe RunnableResult
extractResult tp stream = case runTypeKind tp of
  KindText -> ResultText . LT.toStrict <$> rightToMaybe (LEnc.decodeUtf8' stream)
  KindBin -> Just $ ResultBinary stream
  KindJson -> ResultJson <$> rightToMaybe (eitherDecode stream)

runTypeKind :: RunnableType -> RunnableKind
runTypeKind ScalarImage = KindBin
runTypeKind ScalarGraphic = KindBin
runTypeKind VectorGraphic = KindText
runTypeKind ArbitraryJson = KindJson
runTypeKind ArbitraryText = KindText
runTypeKind TabularCsv = KindText

runTypeName :: RunnableType -> Text
runTypeName ScalarImage = "image"
runTypeName ScalarGraphic = "graphic"
runTypeName VectorGraphic = "vector"
runTypeName ArbitraryJson = "json"
runTypeName ArbitraryText = "text"
runTypeName TabularCsv = "csv"

runTypeExt :: RunnableType -> Text
runTypeExt ScalarImage = "jpg"
runTypeExt ScalarGraphic = "png"
runTypeExt VectorGraphic = "svg"
runTypeExt ArbitraryJson = "json"
runTypeExt ArbitraryText = "txt"
runTypeExt TabularCsv = "csv"

parseTypeName :: Text -> Maybe RunnableType
parseTypeName =
  flip M.lookup $ M.fromList $ (runTypeName &&& id) <$> [minBound .. maxBound]

encodeToJSON :: RunnableResult -> Value
encodeToJSON (ResultText txt) = toJSON txt
encodeToJSON (ResultBinary bin) = toJSON $ extractBase64 $ B64.encodeBase64 bin
encodeToJSON (ResultJson v) = v

encodeToText :: RunnableResult -> Text
encodeToText (ResultText txt) = txt
encodeToText (ResultBinary bin) =
  LT.toStrict $
    LT.intercalate "\n" $
      LT.chunksOf 80 $
        extractBase64 $
          B64.encodeBase64 bin
encodeToText (ResultJson v) = LT.toStrict $ LEnc.decodeUtf8 $ encodePretty v

encodeToLBS :: RunnableResult -> LBS.ByteString
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
    . T.filter (`elem` ['\n', ' '])

decodeJsonFromText :: Text -> Maybe Value
decodeJsonFromText = rightToMaybe . eitherDecode . LEnc.encodeUtf8 . LT.fromStrict

decodeFromText :: RunnableType -> Text -> Maybe RunnableResult
decodeFromText tp txt = case runTypeKind tp of
  KindText -> ResultText <$> decodeTextFromText txt
  KindBin -> ResultBinary <$> decodeBinFromText txt
  KindJson -> ResultJson <$> decodeJsonFromText txt
