{-# LANGUAGE DataKinds #-}

module Data.ERIS.Crypto
  ( ERISBlock,
    ERISCapability (..),
    erisCapBlockSize,
    erisCapLevel,
    erisCapRootRef,
    erisCapRootKey,
    ERISHashAlgorithm,
    ERISHash,
    ERISHashKey,
    mkErisHashKey,
    erisBlake2bKeyed,
    erisBlake2b,
    ERISNonce,
    mkErisNonce,
    mkErisNonceNum,
    erisChaCha20,
    erisPad,
    erisUnpad,
    erisSmallBlockSize,
    erisBlockSize,
  )
where

import Control.Exception (assert)
import Control.Lens
import qualified Crypto.Cipher.ChaCha as ChaCha
import Crypto.Hash
import Crypto.MAC.KeyedBlake2
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8)

checkBits :: (ByteString -> a) -> Int -> ByteString -> Maybe a
checkBits mk sz bs | BS.length bs == sz `div` 8 = Just $ mk bs
checkBits _ _ _ = Nothing

type ERISBlock = ByteString

type ERISHashAlgorithm = Blake2b 256

type ERISHash = Digest ERISHashAlgorithm

data ERISCapability = ERISCapability
  { _erisCapBlockSize :: Int,
    _erisCapLevel :: Word8,
    _erisCapRootRef :: ERISHash,
    _erisCapRootKey :: ERISHash
  }
  deriving (Show)

makeLenses ''ERISCapability

-- Cryptographic Hash Function
newtype ERISHashKey = ERISHashKey {extractHashKey :: ByteString}
  deriving (Show)

mkErisHashKey :: ByteString -> Maybe ERISHashKey
mkErisHashKey = checkBits ERISHashKey 256

erisBlake2bKeyed :: ByteString -> ERISHashKey -> ERISHash
erisBlake2bKeyed input key =
  keyedBlake2GetDigest $ keyedBlake2 (extractHashKey key) input

erisBlake2b :: ByteString -> ERISHash
erisBlake2b = hash

-- Symmetric Key Cipher
newtype ERISNonce = ERISNonce {extractNonce :: ByteString}

mkErisNonce :: ByteString -> Maybe ERISNonce
mkErisNonce = checkBits ERISNonce 96

mkErisNonceNum :: Word8 -> ERISNonce
mkErisNonceNum w = ERISNonce $ BS.cons w $ BS.replicate 11 0x00

erisChaCha20 :: ByteString -> ERISHash -> ERISNonce -> ByteString
erisChaCha20 input key nonce = fst $ ChaCha.combine state input
  where
    state = ChaCha.initialize 20 key (extractNonce nonce)

-- Padding
-- Implementation taken from here:
--   https://eris.codeberg.page/spec/#name-padding-algorithm
erisPad :: ByteString -> Int -> ByteString
erisPad input blockSize = BS.snoc input 0x80 <> pad
  where
    n = BS.length input
    m = (blockSize - ((n + 1) `mod` blockSize)) `mod` blockSize
    pad = BS.replicate m 0x00

erisUnpad :: ByteString -> Int -> ByteString
erisUnpad input blockSize =
  assert (BS.length input >= blockSize) $
    assert (BS.all (== 0x00) pad) $
      BS.dropEnd 1 content
  where
    (content, pad) = BS.spanEnd (/= 0x80) input

erisSmallBlockSize :: Int
erisSmallBlockSize = 1024

erisBlockSize :: Int
erisBlockSize = 32768
