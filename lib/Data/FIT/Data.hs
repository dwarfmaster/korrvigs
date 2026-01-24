module Data.FIT.Data where

import Control.Lens
import Data.Endian
import Data.Int
import Data.Text (Text)
import Data.Word

data FitHeader = FitHeader
  { _fitHdSize :: Word8,
    _fitHdProtocolVersion :: Word8,
    _fitHdProfileVersion :: Word16,
    _fitHdDataSize :: Word32,
    _fitHdCrc :: Maybe Word16
  }
  deriving (Show)

data FitFieldBaseType
  = FitEnum
  | FitSint8
  | FitUint8
  | FitSint16
  | FitUint16
  | FitSint32
  | FitUint32
  | FitString
  | FitFloat32
  | FitFloat64
  | FitUint8Z
  | FitUint16Z
  | FitUint32Z
  | FitByte
  | FitSint64
  | FitUint64
  | FitUint64Z
  deriving (Show, Eq, Ord, Enum, Bounded)

fitTypeNum :: FitFieldBaseType -> Word8
fitTypeNum FitEnum = 0
fitTypeNum FitSint8 = 1
fitTypeNum FitUint8 = 2
fitTypeNum FitSint16 = 3
fitTypeNum FitUint16 = 4
fitTypeNum FitSint32 = 5
fitTypeNum FitUint32 = 6
fitTypeNum FitString = 7
fitTypeNum FitFloat32 = 8
fitTypeNum FitFloat64 = 9
fitTypeNum FitUint8Z = 10
fitTypeNum FitUint16Z = 11
fitTypeNum FitUint32Z = 12
fitTypeNum FitByte = 13
fitTypeNum FitSint64 = 14
fitTypeNum FitUint64 = 15
fitTypeNum FitUint64Z = 16

data FitFieldValue
  = FitEnumV [Word8]
  | FitSint8V [Int8]
  | FitUint8V [Word8]
  | FitSint16V [Int16]
  | FitUint16V [Word16]
  | FitSint32V [Int32]
  | FitUint32V [Word32]
  | FitStringV Text
  | FitFloat32V [Float]
  | FitFloat64V [Double]
  | FitByteV [Word8]
  | FitSint64V [Int64]
  | FitUint64V [Word64]
  deriving (Show)

data FitFieldType = FitFieldType
  { _fitFieldTpSingleByte :: Bool,
    _fitFieldTpBase :: FitFieldBaseType
  }
  deriving (Show)

data FitFieldDefinition = FitFieldDefinition
  { _fitFieldDefNumber :: Word8,
    _fitFieldDefSize :: Word8,
    _fitFieldDefType :: FitFieldType
  }
  deriving (Show)

data FitDefinitionData = FitDefinitionData
  { _fitDefArchitecture :: Endian,
    _fitDefGlobalMsg :: Word16,
    _fitDefData :: [FitFieldDefinition]
  }
  deriving (Show)

type FitLocalMsgType = Word8

data FitData
  = FitDefinition FitDefinitionData
  | FitData [FitFieldValue]
  deriving (Show)

data FitRecordNormal = FitRecordNormal
  { _fitRecLocalType :: FitLocalMsgType,
    _fitRecData :: FitData
  }
  deriving (Show)

data FitRecordTimestamp = FitRecordTimestamp
  { _fitRecTimeLocalType :: FitLocalMsgType,
    _fitRecTimeOffset :: Word8,
    _fitRecTimeData :: [FitFieldValue]
  }
  deriving (Show)

data FitRecord
  = FitNormal FitRecordNormal
  | FitTimestamp FitRecordTimestamp
  deriving (Show)

data FitFile = FitFile
  { _fitHeader :: FitHeader,
    _fitCRC :: Word16
  }
  deriving (Show)

makeLenses ''FitHeader
makePrisms ''FitFieldBaseType
makeLenses ''FitFieldType
makeLenses ''FitFieldDefinition
makeLenses ''FitDefinitionData
makePrisms ''FitData
makeLenses ''FitRecordNormal
makeLenses ''FitRecordTimestamp
makePrisms ''FitRecord
makeLenses ''FitFile
