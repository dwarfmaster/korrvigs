module Data.ERIS.StreamingEncode where

import Conduit
import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as CC
import Data.ERIS.Crypto
import Data.ERIS.DB.Class
import qualified Data.ERIS.Encode as Encode
import Data.Maybe
import Data.Word (Word8)

data ERISEncodeState = ERISEncodeState
  { _encodedBlocks :: [(ERISHash, ERISHash)],
    _numBlocks :: Int,
    _level :: Word8,
    _parentState :: Maybe ERISEncodeState
  }

makeLenses ''ERISEncodeState

mkState :: Word8 -> ERISEncodeState
mkState lvl =
  ERISEncodeState
    { _encodedBlocks = [],
      _numBlocks = 0,
      _level = lvl,
      _parentState = Nothing
    }

erisEncodeStreaming ::
  (ERISBlockWrite db m) =>
  db ->
  ConvergenceSecret ->
  Int ->
  ConduitT ByteString Void m ERISCapability
erisEncodeStreaming db conv blockSize =
  erisDataSuffix
    .| CC.chunksOfE blockSize
    .| erisStreamPad blockSize
    .| erisEncodeBlock db conv blockSize (mkState 0)

erisDataSuffix :: (Monad m) => ConduitT ByteString ByteString m ()
erisDataSuffix = CC.takeWhile (const True) >> yield (BS.singleton 0x80)

erisStreamPad :: (Monad m) => Int -> ConduitT ByteString ByteString m ()
erisStreamPad sz = CC.map streamPad
  where
    streamPad bs | BS.length bs < sz = bs <> BS.replicate (sz - BS.length bs) 0x00
    streamPad bs = bs

erisEncodeBlock ::
  (ERISBlockWrite db m) =>
  db ->
  ConvergenceSecret ->
  Int ->
  ERISEncodeState ->
  ConduitT ByteString Void m ERISCapability
erisEncodeBlock db conv blockSize state =
  await >>= \case
    Nothing ->
      lift $ getRoot <$> commitState db conv blockSize True state
    Just content -> do
      nstate <- lift $ pushBlock db conv blockSize False state content
      erisEncodeBlock db conv blockSize nstate
  where
    getRoot st = case st ^. parentState of
      Nothing -> case st ^. encodedBlocks of
        [(rootRef, rootKey)] ->
          ERISCapability
            { _erisCapBlockSize = blockSize,
              _erisCapLevel = st ^. level,
              _erisCapRootRef = rootRef,
              _erisCapRootKey = rootKey
            }
        [] -> error "Empty state"
        _ -> error "More than one root"
      Just parent -> getRoot parent

pushBlock ::
  (ERISBlockWrite db m) =>
  db ->
  ConvergenceSecret ->
  Int ->
  Bool ->
  ERISEncodeState ->
  ByteString ->
  m ERISEncodeState
pushBlock db conv blockSize force state content = do
  let lvl = state ^. level
  let (block, ref, key) =
        if lvl > 0
          then Encode.erisEncryptInternalNode content lvl
          else Encode.erisEncryptLeafNode content conv
  erisBlockStoragePut db ref block
  let nstate =
        ERISEncodeState
          { _encodedBlocks = (ref, key) : state ^. encodedBlocks,
            _level = lvl,
            _numBlocks = 1 + state ^. numBlocks,
            _parentState = state ^. parentState
          }
  commitState db conv blockSize force nstate

commitState ::
  (ERISBlockWrite db m) =>
  db ->
  ConvergenceSecret ->
  Int ->
  Bool ->
  ERISEncodeState ->
  m ERISEncodeState
commitState db conv blockSize force state
  | (force && state ^. numBlocks > 1) || state ^. numBlocks >= arity = do
      let lvl = state ^. level
      let ncontent = Encode.chunksToNode blockSize $ reverse $ state ^. encodedBlocks
      let pState = fromMaybe (mkState $ lvl + 1) $ state ^. parentState
      nparent <- pushBlock db conv blockSize force pState ncontent
      pure $ mkState lvl & parentState ?~ nparent
  where
    arity = blockSize `div` 64
commitState _ _ _ _ state = pure state
