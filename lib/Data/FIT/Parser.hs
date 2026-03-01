module Data.FIT.Parser where

import Conduit
import Control.Arrow
import Control.Lens
import Control.Monad
import Control.Monad.Extra
import Control.Monad.Loops
import Data.Binary.Get
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Endian
import Data.FIT.Data
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import Data.Word

repM :: (Monad m) => Word8 -> Word8 -> m a -> m [a]
repM n step = replicateM (fromInteger $ toInteger $ n `div` step)

-- Parsing
data ParsingStep
  = ParseHeader (Decoder FitHeader)
  | ParseData (Decoder FitRecord)
  | ParseCRC (Decoder Word16)
  | ParseFail Text

data ParsingState = ParsingState
  { _step :: ParsingStep,
    _messages :: Map Word8 [(Endian, FitFieldBaseType, Word8)],
    _remainingSize :: Word32
  }

makeLenses ''ParsingState

fitParser :: (MonadIO m) => Maybe FilePath -> ConduitT ByteString FitRecord m ()
fitParser mpath = do
  state <-
    liftIO $
      newIORef $
        ParsingState
          (ParseHeader $ runGetIncremental getHeader)
          M.empty
          0
  void $ whileJust (next state) $ \input ->
    liftIO (readIORef state) >>= runParserOn yield input >>= liftIO . writeIORef state
  where
    next state =
      liftIO (readIORef state) >>= \st -> case st ^. step of
        ParseFail msg -> do
          liftIO $ putStrLn $ fromMaybe "<>" mpath <> ": " <> T.unpack msg
          pure Nothing
        _ -> await

runParserOn :: (Monad m) => (FitRecord -> m ()) -> ByteString -> ParsingState -> m ParsingState
runParserOn _ input state | BS.null input = pure state
runParserOn doYield input state = case state ^. step of
  ParseFail msg -> pure $ state & step .~ ParseFail msg
  ParseHeader dec -> case pushChunk dec input of
    Fail _ _ msg -> pure $ state & step .~ ParseFail (T.pack msg)
    ndec@(Partial _) -> pure $ state & step .~ ParseHeader ndec
    Done remain _ hd ->
      runParserOn doYield remain $
        state
          & step .~ ParseData (runGetIncremental $ getRecord $ const Nothing)
          & messages .~ M.empty
          & remainingSize .~ hd ^. fitHdDataSize
  ParseCRC dec -> case pushChunk dec input of
    Fail _ _ msg -> pure $ state & step .~ ParseFail (T.pack msg)
    ndec@(Partial _) -> pure $ state & step .~ ParseCRC ndec
    Done remain _ _ ->
      runParserOn doYield remain $
        state
          & step .~ ParseHeader (runGetIncremental getHeader)
          & messages .~ M.empty
          & remainingSize .~ 0
  ParseData dec -> case pushChunk dec input of
    Fail _ _ msg -> pure $ state & step .~ ParseFail (T.pack msg)
    ndec@(Partial _) -> do
      let remainSize = state ^. remainingSize - fromInteger (toInteger $ BS.length input)
      pure $
        state
          & step .~ ParseData ndec
          & remainingSize .~ remainSize
    Done remain _ dat -> do
      let consumed = BS.length input - BS.length remain
      let remainSize = state ^. remainingSize - fromInteger (toInteger consumed)
      let updMsgs = fromMaybe id $ do
            normal <- dat ^? _FitNormal
            def <- normal ^? fitRecData . _FitDefinition
            pure $ M.insert (normal ^. fitRecLocalType) $ defMsg def
      let nmsgs = state ^. messages . to updMsgs
      let nextStep =
            if remainSize == 0
              then ParseCRC $ runGetIncremental getWord16le
              else ParseData $ runGetIncremental $ getRecord $ flip M.lookup nmsgs
      let nstate =
            state
              & step .~ nextStep
              & messages .~ nmsgs
              & remainingSize .~ remainSize
      doYield dat
      runParserOn doYield remain nstate
  where
    defMsg :: FitDefinitionData -> [(Endian, FitFieldBaseType, Word8)]
    defMsg d = (\dat -> (d ^. fitDefArchitecture, dat ^. fitFieldDefType . fitFieldTpBase, dat ^. fitFieldDefSize)) <$> d ^. fitDefData

-- Parsers
end :: Endian -> a -> a -> a
end LittleEndian le _ = le
end BigEndian _ be = be

getWord16 :: Endian -> Get Word16
getWord16 e = end e getWord16le getWord16be

getHeader :: Get FitHeader
getHeader = do
  hdSize <- getWord8
  protVersion <- getWord8
  profVersion <- getWord16le
  dataSize <- getWord32le
  replicateM_ 4 getWord8
  crc <- whenMaybe (hdSize >= 14) getWord16le
  when (hdSize > 14) $ replicateM_ (fromInteger (toInteger hdSize) - 14) getWord8
  pure $
    FitHeader
      { _fitHdSize = hdSize,
        _fitHdProtocolVersion = protVersion,
        _fitHdProfileVersion = profVersion,
        _fitHdDataSize = dataSize,
        _fitHdCrc = crc
      }

getRecord :: (Word8 -> Maybe [(Endian, FitFieldBaseType, Word8)]) -> Get FitRecord
getRecord getMsg = do
  header <- getWord8
  if not (testBit header 7)
    then FitNormal <$> getNormalData getMsg header
    else FitTimestamp <$> getTimeData getMsg header

getNormalData :: (Word8 -> Maybe [(Endian, FitFieldBaseType, Word8)]) -> Word8 -> Get FitRecordNormal
getNormalData getMsg header = do
  dat <-
    if isDef
      then FitDefinition <$> getDefinition devFlag
      else FitData <$> getData getMsg localType
  pure $
    FitRecordNormal
      { _fitRecLocalType = localType,
        _fitRecData = dat
      }
  where
    isDef = testBit header 6
    devFlag = testBit header 5
    localType = header .&. 0x0F

getTimeData :: (Word8 -> Maybe [(Endian, FitFieldBaseType, Word8)]) -> Word8 -> Get FitRecordTimestamp
getTimeData getMsg header = do
  dat <- getData getMsg localType
  pure $
    FitRecordTimestamp
      { _fitRecTimeLocalType = localType,
        _fitRecTimeOffset = tz,
        _fitRecTimeData = dat
      }
  where
    localType = shiftL (header .&. 0x60) 5
    tz = header .&. 0x1F

getData :: (Word8 -> Maybe [(Endian, FitFieldBaseType, Word8)]) -> Word8 -> Get [FitFieldValue]
getData getMsg localType = do
  msg <- maybe (fail $ "Unknown local type: " <> show localType) pure (getMsg localType)
  forM msg $ \(e, tp, sz) -> getFieldValue e tp sz

utf8NullTerminated :: Get Text
utf8NullTerminated =
  fmap (Enc.decodeUtf8Lenient . BS.pack) $ unfoldM $ do
    w <- getWord8
    pure $ if w == 0 then Nothing else Just w

getFieldValue :: Endian -> FitFieldBaseType -> Word8 -> Get FitFieldValue
getFieldValue _ FitEnum sz = FitEnumV <$> repM sz 1 getWord8
getFieldValue _ FitSint8 sz = FitSint8V <$> repM sz 1 getInt8
getFieldValue _ FitUint8 sz = FitUint8V <$> repM sz 1 getWord8
getFieldValue e FitSint16 sz = FitSint16V <$> repM sz 2 (end e getInt16le getInt16be)
getFieldValue e FitUint16 sz = FitUint16V <$> repM sz 2 (end e getWord16le getWord16be)
getFieldValue e FitSint32 sz = FitSint32V <$> repM sz 4 (end e getInt32le getInt32be)
getFieldValue e FitUint32 sz = FitUint32V <$> repM sz 4 (end e getWord32le getWord32be)
getFieldValue _ FitString _ = FitStringV <$> utf8NullTerminated
getFieldValue e FitFloat32 sz = FitFloat32V <$> repM sz 4 (end e getFloatle getFloatbe)
getFieldValue e FitFloat64 sz = FitFloat64V <$> repM sz 8 (end e getDoublele getDoublebe)
getFieldValue _ FitUint8Z sz = FitUint8V <$> repM sz 1 getWord8
getFieldValue e FitUint16Z sz = FitUint16V <$> repM sz 2 (end e getWord16le getWord16be)
getFieldValue e FitUint32Z sz = FitUint32V <$> repM sz 4 (end e getWord32le getWord32be)
getFieldValue _ FitByte sz = FitByteV <$> repM sz 1 getWord8
getFieldValue e FitSint64 sz = FitSint64V <$> repM sz 8 (end e getInt64le getInt64be)
getFieldValue e FitUint64 sz = FitUint64V <$> repM sz 8 (end e getWord64le getWord64be)
getFieldValue e FitUint64Z sz = FitUint64V <$> repM sz 8 (end e getWord64le getWord64be)

getDefinition :: Bool -> Get FitDefinitionData
getDefinition devFlag = do
  void getWord8
  arch <- getWord8
  let endian = if arch == 0 then LittleEndian else BigEndian
  globalMsg <- getWord16 endian
  numFields <- getWord8
  fields <- replicateM (fromInteger $ toInteger numFields) getField
  when devFlag $ do
    numDev <- getWord8
    replicateM_ (3 * fromInteger (toInteger numDev)) getWord8
  pure $
    FitDefinitionData
      { _fitDefArchitecture = endian,
        _fitDefGlobalMsg = globalMsg,
        _fitDefData = fields
      }

getField :: Get FitFieldDefinition
getField = do
  defNumber <- getWord8
  defSize <- getWord8
  tp <- getWord8
  baseTp <- tpGetNum $ tp .&. 0x1F
  pure $
    FitFieldDefinition
      { _fitFieldDefNumber = defNumber,
        _fitFieldDefSize = defSize,
        _fitFieldDefType =
          FitFieldType
            { _fitFieldTpSingleByte = not $ testBit tp 7,
              _fitFieldTpBase = baseTp
            }
      }
  where
    tpNums :: Map Word8 FitFieldBaseType
    tpNums = M.fromList $ (fitTypeNum &&& id) <$> [minBound .. maxBound]
    tpGetNum :: Word8 -> Get FitFieldBaseType
    tpGetNum tp = case M.lookup tp tpNums of
      Nothing -> fail $ "Unknown field type: " <> show tp
      Just fitTp -> pure fitTp
