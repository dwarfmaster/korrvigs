module Data.ERIS.Decode where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Writer
import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Bld
import qualified Data.ByteString.Lazy as LBS
import Data.ERIS.Crypto
import Data.ERIS.DB.Class
import Data.List (unfoldr)
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
    then
      let dat = if isRightmost then erisUnpad node blockSize else node
       in doYield dat
    else do
      subNodes <-
        forM (refKeyPairs node) $ \(ref, ky, rightmost) ->
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
    decodeOneRefKey :: ByteString -> Maybe ((ERISHash, ERISHash, Bool), ByteString)
    decodeOneRefKey bs | BS.null bs = Nothing
    decodeOneRefKey bs = do
      let (pair, rest) = BS.splitAt 64 bs
      (ref, ky) <- decodeRefKey pair
      pure ((ref, ky, BS.null rest), rest)
    refKeyPairs :: ByteString -> [(ERISHash, ERISHash, Bool)]
    refKeyPairs = unfoldr decodeOneRefKey

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
