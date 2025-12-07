module Data.ERIS.Decode where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Loops (unfoldrM)
import Control.Monad.Trans.Writer
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Lazy as LBS
import Data.ERIS.Crypto
import Data.ERIS.DB.Class
import Data.Word (Word8)

erisDecodeRecurse ::
  forall db m.
  (ERISBlockRead db m, MonadFail m) =>
  db ->
  (ByteString -> m ()) ->
  Int ->
  Word8 ->
  Bool -> -- Indicates if block is right-most at this level
  ERISHash ->
  ERISHash ->
  m ()
erisDecodeRecurse db doYield blockSize level isRightmost reference key = do
  node <- erisDereferenceNode db reference key level blockSize
  if level == 0
    then do
      dat <- if isRightmost then erisUnpad node blockSize else pure node
      unless (BS.null dat) $ doYield dat
    else do
      pairs <- refKeyPairs node
      subNodes <-
        forM pairs $ \(ref, ky, rightmost) ->
          erisDecodeRecurse db doYield blockSize (level - 1) (isRightmost && rightmost) ref ky
      pure $ mconcat subNodes
  where
    decodeRefKey :: ByteString -> Maybe (ERISHash, ERISHash)
    decodeRefKey bs | BS.all (== 0x00) bs = Nothing
    decodeRefKey bs = do
      let (ref, ky) = BS.splitAt 32 bs
      hashRef <- digestFromByteString ref
      hashKey <- digestFromByteString ky
      pure (hashRef, hashKey)
    decodeOneRefKey :: ByteString -> m (Maybe ((ERISHash, ERISHash, Bool), ByteString))
    decodeOneRefKey bs | BS.null bs = pure Nothing
    decodeOneRefKey bs =
      let (pair, rest) = BS.splitAt 64 bs
       in case decodeRefKey pair of
            Nothing ->
              if BS.all (== 0x00) rest
                then pure Nothing
                else fail "corrupted internal node"
            Just (ref, ky) ->
              pure $ Just ((ref, ky, BS.null rest || BS.all (== 0x00) (BS.take 64 rest)), rest)
    refKeyPairs :: ByteString -> m [(ERISHash, ERISHash, Bool)]
    refKeyPairs = unfoldrM decodeOneRefKey

erisDecode :: (ERISBlockRead db m, MonadFail m) => db -> ERISCapability -> m LBS.ByteString
erisDecode db cap = do
  content <- execWriterT $ erisDecode' db (tell . Bld.byteString) cap
  pure $ Bld.toLazyByteString content

erisDecodeStreaming :: (ERISBlockRead db m, MonadFail m) => db -> ERISCapability -> ConduitT i ByteString m ()
erisDecodeStreaming = flip erisDecode' yield

erisDecode' :: (ERISBlockRead db m, MonadFail m) => db -> (ByteString -> m ()) -> ERISCapability -> m ()
erisDecode' db doYield cap = do
  when (level > 0) $ erisVerifyKey db level rootReference rootKey blockSize
  erisDecodeRecurse db doYield blockSize level True rootReference rootKey
  where
    level = cap ^. erisCapLevel
    blockSize = cap ^. erisCapBlockSize
    rootReference = cap ^. erisCapRootRef
    rootKey = cap ^. erisCapRootKey

erisDereferenceNode :: (ERISBlockRead db m, MonadFail m) => db -> ERISHash -> ERISHash -> Word8 -> Int -> m ByteString
erisDereferenceNode db reference key level blockSize = do
  block <- erisBlockStorageGet db reference >>= maybe (fail "block not in database") pure
  when (BS.length block /= blockSize) $ fail "block has invalid size"
  when (erisBlake2b block /= reference) $ fail "block is invalid"
  let nonce = mkErisNonceNum level
  pure $ erisChaCha20 block key nonce

erisVerifyKey :: (ERISBlockRead db m, MonadFail m) => db -> Word8 -> ERISHash -> ERISHash -> Int -> m ()
erisVerifyKey db level reference key blockSize = do
  when (level == 0) $ fail "cannot verify a leaf node"
  node <- erisDereferenceNode db reference key level blockSize
  let hsh = erisBlake2b node
  when (hsh /= key) $ fail "key in read capability is invalid"
