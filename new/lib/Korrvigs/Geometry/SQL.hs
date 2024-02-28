{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Geometry.SQL (SqlGeometry) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Binary
import Data.Binary.Builder (fromLazyByteString)
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lex.Integral
import Data.Endian
import Data.Maybe (fromJust)
import Data.Profunctor (lmap)
import Data.Profunctor.Product.Default (Default, def)
import Database.PostgreSQL.Simple.FromField hiding (Field)
import Database.PostgreSQL.Simple.ToField
import Korrvigs.Geometry.Def
import Linear.V2
import Opaleye (DefaultFromField, Field, SqlBytea, ToFields, defaultFromField)

-- Opaleye
type SqlGeometry = SqlBytea

instance DefaultFromField SqlBytea Geometry where
  defaultFromField = readGeometry <$> defaultFromField

instance Default ToFields Geometry (Field SqlBytea) where
  def = lmap writeGeometry def

-- Postgresql simple
instance ToField Geometry where
  toField = Plain . fromLazyByteString . writeGeometry

instance FromField Geometry where
  fromField _ (Just bs) = pure $ readGeometry $ BL.fromStrict bs
  fromField _ Nothing = error "Invalid field"

-- Parsers
type SRID = Maybe Int

data Header = Header
  { _byteOrder :: Endian,
    _geoType :: Word32,
    _srid :: SRID
  }
  deriving (Show)

type Getter a = ReaderT Header Get a

type Putter a = ReaderT Endian (Writer Put) a

wkbZ, wkbM, wkbSRID :: Word32
wkbZ = 0x80000000
wkbM = 0x40000000
wkbSRID = 0x20000000

ewkbTypeOffset :: Word32
ewkbTypeOffset = 0x1fffffff

writeGeometry :: Geometry -> BL.ByteString
writeGeometry = runPut . putGeometry

readGeometry :: BL.ByteString -> Geometry
readGeometry = runGet getGeometry

-- Endianness
putEndian :: Endian -> Put
putEndian BigEndian = putLazyByteString "00"
putEndian LittleEndian = putLazyByteString "01"

getEndian :: Get Endian
getEndian =
  (getHexInt 2 :: Get Int) >>= \case
    0 -> pure BigEndian
    1 -> pure LittleEndian
    n -> fail $ "Unknown endianness encoding " <> show n

putInt' :: Integral a => Endian -> a -> Put
putInt' bo = putByteString . fromJust . packHexadecimal . reEndian . fromIntegral
  where
    reEndian :: Word32 -> Word32
    reEndian = toEndian bo

putMaybe' :: (a -> Put) -> Maybe a -> Put
putMaybe' = maybe (pure ())

-- Header
instance Binary Header where
  put (Header bo tp sr) = do
    putEndian bo
    putInt' bo tp
    putMaybe' (putInt' bo) sr
  get = getHeader

getWord32' :: Endian -> Get Word32
getWord32' BigEndian = fromBigEndian <$> getWord32be
getWord32' LittleEndian = fromLittleEndian <$> getWord32le

getInt' :: Endian -> Get Int
getInt' BigEndian = fromIntegral <$> getInt32be
getInt' LittleEndian = fromIntegral <$> getInt32le

getHeader :: Get Header
getHeader = do
  bo <- getEndian
  t <- getWord32' bo
  Header bo t <$> getSRID bo t
  where
    getSRID :: Endian -> Word32 -> Get SRID
    getSRID bo t | t .&. wkbSRID > 0 = Just <$> getInt' bo
    getSRID _ _ = pure Nothing

-- Getters
getHexInt :: Integral a => Int -> Get a
getHexInt n = getByteString n >>= getHexa . readHexadecimal
  where
    getHexa (Just (v, _)) = pure v
    getHexa Nothing = fail "Cannot parse hexadecimal"

getGeometry :: Get Geometry
getGeometry = do
  h <- get
  let t = _geoType h .&. ewkbTypeOffset
      mkGeo :: (a -> Geometry) -> Getter a -> Get Geometry
      mkGeo constr p = constr <$> runReaderT p h
  case t of
    1 -> mkGeo GeoPoint getPoint
    2 -> mkGeo GeoPath getPath
    3 -> mkGeo GeoPolygon getPolygon
    _ -> fail $ "Unsupported geotype " <> show t

getPoint :: Getter Point
getPoint = do
  gt <- asks _geoType
  let hasM = (gt .&. wkbM) > 0
      hasZ = (gt .&. wkbZ) > 0
  x <- getDouble
  y <- getDouble
  when hasZ $ void getDouble
  when hasM $ void getDouble
  pure $ V2 x y

getWithEndiannes :: (Endian -> Get a) -> Getter a
getWithEndiannes f = asks _byteOrder >>= lift . f

getInt :: Getter Int
getInt = getWithEndiannes getInt'

getDouble :: Getter Double
getDouble = getWithEndiannes $ \case
  BigEndian -> getDoublebe
  LittleEndian -> getDoublele

getPath :: Getter Path
getPath = do
  n <- getInt
  replicateM n getPoint

getPolygon :: Getter Polygon
getPolygon = do
  n <- getInt
  rings <- replicateM n getPath
  case rings of
    [] -> fail "Expected at least one ring in a polygon"
    outer : inners -> pure $ Polygon outer inners

-- Putters
runPutter :: Endian -> Putter () -> Put
runPutter bo pt = snd $ runWriter $ runReaderT pt bo

putEndianness :: Endian
putEndianness = LittleEndian

withHeader :: Int -> Putter () -> Put
withHeader i pt = do
  put $ Header putEndianness (fromIntegral i) Nothing
  runPutter putEndianness pt

putGeometry :: Geometry -> Put
putGeometry (GeoPoint pt) = withHeader 1 $ putPoint pt
putGeometry (GeoPath pth) = withHeader 2 $ putPath pth
putGeometry (GeoPolygon poly) = withHeader 3 $ putPolygon poly

putWithEndianness :: (Endian -> a -> Put) -> a -> Putter ()
putWithEndianness f a = ask >>= \bo -> tell $ f bo a

putInt :: Int -> Putter ()
putInt = putWithEndianness putInt'

putDouble :: Double -> Putter ()
putDouble = putWithEndianness $ \case
  BigEndian -> putDoublebe
  LittleEndian -> putDoublele

putPoint :: Point -> Putter ()
putPoint (V2 x y) = putDouble x >> putDouble y

putPath :: Path -> Putter ()
putPath pth = putInt (length pth) >> mapM_ putPoint pth

putPolygon :: Polygon -> Putter ()
putPolygon (Polygon outer inners) = do
  putInt $ 1 + length inners
  mapM_ putPath $ outer : inners
