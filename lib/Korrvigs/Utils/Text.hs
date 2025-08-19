module Korrvigs.Utils.Text where

import Codec.Text.IConv
import qualified Data.ByteString.Lazy as LBS
import Data.Char
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc

toASCIIFuzzy :: Text -> Text
toASCIIFuzzy = Enc.decodeASCII . LBS.toStrict . convertFuzzy Transliterate "utf-8" "ascii" . LBS.fromStrict . Enc.encodeUtf8

sanitize :: Char -> Char
sanitize c | isAlpha c = c
sanitize _ = ' '
