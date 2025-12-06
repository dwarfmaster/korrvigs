{-# LANGUAGE DataKinds #-}

module Data.ERIS.Crypto
  ( ERISBlock,
    ERISHashAlgorithm,
    ERISHash,
    ERISHashKey,
    mkErisHashKey,
    erisBlake2bKeyed,
    erisBlake2b,
    ERISKey,
    mkErisKey,
    ERISNonce,
    mkErisNonce,
    erisChaCha20,
    erisPad,
    erisUnpad,
    erisSmallBlockSize,
    erisBlockSize,
  )
where

import Control.Exception (assert)
import qualified Crypto.Cipher.ChaCha as ChaCha
import Crypto.Hash
import Crypto.MAC.KeyedBlake2
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

checkBits :: (ByteString -> a) -> Int -> ByteString -> Maybe a
checkBits mk sz bs | BS.length bs == sz `div` 8 = Just $ mk bs
checkBits _ _ _ = Nothing

type ERISBlock = ByteString

-- Cryptographic Hash Function
type ERISHashAlgorithm = Blake2b 256

type ERISHash = Digest ERISHashAlgorithm

newtype ERISHashKey = ERISHashKey {extractHashKey :: ByteString}

mkErisHashKey :: ByteString -> Maybe ERISHashKey
mkErisHashKey = checkBits ERISHashKey 256

erisBlake2bKeyed :: ByteString -> ERISHashKey -> ERISHash
erisBlake2bKeyed input key =
  keyedBlake2GetDigest $ keyedBlake2 (extractHashKey key) input

erisBlake2b :: ByteString -> ERISHash
erisBlake2b = hash

-- Symmetric Key Cipher
newtype ERISKey = ERISKey {extractKey :: ByteString}

newtype ERISNonce = ERISNonce {extractNonce :: ByteString}

mkErisKey :: ByteString -> Maybe ERISKey
mkErisKey = checkBits ERISKey 256

mkErisNonce :: ByteString -> Maybe ERISNonce
mkErisNonce = checkBits ERISNonce 96

erisChaCha20 :: ByteString -> ERISKey -> ERISNonce -> ByteString
erisChaCha20 input key nonce = fst $ ChaCha.combine state input
  where
    state = ChaCha.initialize 20 (extractKey key) (extractNonce nonce)

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
