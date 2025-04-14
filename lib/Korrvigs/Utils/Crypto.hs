{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Utils.Crypto where

import Crypto.Hash
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Base64
import Data.Maybe
import Data.Profunctor
import Data.Profunctor.Product.Default
import Data.Text (Text)
import qualified Data.Text.Encoding as Enc
import Opaleye

digestToText :: Digest a -> Text
digestToText = encodeBase64 . BS.pack . BA.unpack

digestToHexa :: Digest a -> Text
digestToHexa = Enc.decodeASCII . B16.encode . BS.pack . BA.unpack

digestFromText :: (HashAlgorithm a) => Text -> Maybe (Digest a)
digestFromText = digestFromByteString . decodeBase64Lenient . Enc.encodeUtf8

digestFromHexa :: (HashAlgorithm a) => Text -> Maybe (Digest a)
digestFromHexa = digestFromByteString . B16.decodeLenient . Enc.encodeUtf8

instance (HashAlgorithm a) => DefaultFromField SqlText (Digest a) where
  defaultFromField = extract . digestFromText <$> defaultFromField
    where
      extract = fromMaybe $ error "Invalid hash in database"

instance Default ToFields (Digest a) (Field SqlText) where
  def = lmap digestToText def
