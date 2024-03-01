{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Geometry.SQL where

import qualified Data.ByteString.Lazy as BL
import Data.List (singleton)
import Data.Profunctor.Product.Default (Default, def)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeLatin1)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Korrvigs.Geometry.Def
import Korrvigs.Geometry.WKB
import Opaleye hiding (FromField)
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import Opaleye.Internal.PGTypes (IsSqlType, showSqlType)
import qualified Opaleye.Internal.PGTypes as IPT

data SqlGeometry

instance IsSqlType SqlGeometry where
  showSqlType _ = "geography"

pgGeometry :: BL.ByteString -> Field SqlGeometry
pgGeometry = IPT.literalColumn . HPQ.StringLit . TL.unpack . decodeLatin1

sqlGeometry :: Geometry -> Field SqlGeometry
sqlGeometry = pgGeometry . writeGeometry

instance FromField Geometry where
  fromField _ Nothing = error "Invalid geometry field"
  fromField _ (Just bs) = pure $ readGeometry $ BL.fromStrict bs

instance DefaultFromField SqlGeometry Geometry where
  defaultFromField = fromPGSFromField

instance Default ToFields Geometry (Field SqlGeometry) where
  def = toToFields sqlGeometry

-- Conversion
sqlPoint :: Point -> Field SqlGeometry
sqlPoint = toFields . GeoPoint

sqlPath :: Path -> Field SqlGeometry
sqlPath = toFields . GeoPath

sqlPolygon :: Polygon -> Field SqlGeometry
sqlPolygon = toFields . GeoPolygon

-- Operators
ap1 :: String -> Field a -> Field b
ap1 f = C.Column . HPQ.FunExpr f . singleton . C.unColumn

ap2 :: String -> Field a -> Field b -> Field c
ap2 f (C.Column a) (C.Column b) = C.Column $ HPQ.FunExpr f [a, b]

ap3 :: String -> Field a -> Field b -> Field c -> Field d
ap3 f (C.Column a) (C.Column b) (C.Column c) = C.Column $ HPQ.FunExpr f [a, b, c]

stAsText :: Field SqlGeometry -> Field SqlText
stAsText = ap1 "ST_AsText"

stArea :: Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stArea = ap2 "ST_Area"

stAzimuth :: Field SqlGeometry -> Field SqlGeometry -> Field SqlFloat8
stAzimuth = ap2 "ST_Azimuth"

stDistance :: Field SqlGeometry -> Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stDistance = ap3 "ST_Distance"

stLength :: Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stLength = ap2 "ST_Length"

stPerimeter :: Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stPerimeter = ap2 "ST_Perimeter"

stProject :: Field SqlGeometry -> Field SqlFloat8 -> Field SqlFloat8 -> Field SqlGeometry
stProject = ap3 "ST_Project"
