module Data.ERIS.Encode where

import Control.Monad
import Control.Monad.Writer.Lazy
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Lazy as LBS
import Data.ERIS.Crypto
import Data.List (singleton, unfoldr)
import qualified Data.List.Split as Split
import Data.Word (Word8)

type ConvergenceSecret = ERISHashKey

type MonadEncode = (,) [ERISBlock]

pushBlock :: ERISBlock -> MonadEncode ()
pushBlock = tell . singleton

erisEncode :: ByteString -> ConvergenceSecret -> Int -> (ERISCapability, [ERISBlock])
erisEncode content convergenceSecret blockSize =
  ( ERISCapability
      { _erisCapBlockSize = blockSize,
        _erisCapLevel = rootLevel,
        _erisCapRootRef = rootReference,
        _erisCapRootKey = rootKey
      },
    blocks
  )
  where
    leafNodes = erisSplitContent content blockSize
    prepareLeafNode :: ByteString -> MonadEncode (ERISHash, ERISHash)
    prepareLeafNode leafNode = do
      let (block, reference, key) = erisEncryptLeafNode leafNode convergenceSecret
      pushBlock block
      pure (reference, key)
    leafReferenceKeyPairs = mapM prepareLeafNode leafNodes
    prepareInternalNode :: Word8 -> ByteString -> MonadEncode (ERISHash, ERISHash)
    prepareInternalNode level node = do
      let (block, reference, key) = erisEncryptInternalNode node level
      pushBlock block
      pure (reference, key)
    recurseLevels :: Word8 -> [(ERISHash, ERISHash)] -> MonadEncode (Word8, ERISHash, ERISHash)
    recurseLevels _ [] = error "Empty list of ERIS nodes"
    recurseLevels level [(reference, key)] = pure (level - 1, reference, key)
    recurseLevels level referenceKeyPairs = do
      let nodes = erisConstructInternalNodes referenceKeyPairs blockSize
      newReferenceKeyPairs <- mapM (prepareInternalNode level) nodes
      recurseLevels (level + 1) newReferenceKeyPairs
    (blocks, (rootLevel, rootReference, rootKey)) =
      recurseLevels 1 =<< leafReferenceKeyPairs

erisSplitContent :: ByteString -> Int -> [ByteString]
erisSplitContent content blockSize = chunksOf blockSize $ erisPad content blockSize
  where
    chunksOf n = unfoldr (\bs -> guard (not $ BS.null bs) >> pure (BS.splitAt n bs))

erisEncryptLeafNode :: ByteString -> ConvergenceSecret -> (ERISBlock, ERISHash, ERISHash)
erisEncryptLeafNode node convergenceSecret = (block, reference, key)
  where
    key = erisBlake2bKeyed node convergenceSecret
    nonce = mkErisNonceNum 0x00
    block = erisChaCha20 node key nonce
    reference = erisBlake2b block

erisEncryptInternalNode :: ByteString -> Word8 -> (ERISBlock, ERISHash, ERISHash)
erisEncryptInternalNode node level = (block, reference, key)
  where
    key = erisBlake2b node
    nonce = mkErisNonceNum level
    block = erisChaCha20 node key nonce
    reference = erisBlake2b block

erisConstructInternalNodes :: [(ERISHash, ERISHash)] -> Int -> [ByteString]
erisConstructInternalNodes referenceKeyPairs blockSize = chunksToNode <$> chunks
  where
    arity = blockSize `div` 64
    chunks = Split.chunksOf arity referenceKeyPairs
    chunksToNode :: [(ERISHash, ERISHash)] -> ByteString
    chunksToNode pairs =
      let bld = foldMap (\(ref, k) -> toBld ref <> toBld k) pairs
       in let bs = LBS.toStrict $ Bld.toLazyByteString bld
           in let n = blockSize - BS.length bs
               in bs <> BS.replicate n 0x00
    toBld :: ERISHash -> Bld.Builder
    toBld = foldMap Bld.word8 . BA.unpack
