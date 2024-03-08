{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Geometry.SQL where

import qualified Data.ByteString.Lazy as BL
import Data.Profunctor.Product.Default (Default, def)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeLatin1)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Korrvigs.Geometry.Def
import Korrvigs.Geometry.WKB
import qualified Korrvigs.Utils.Opaleye as UOp
import Opaleye hiding (FromField)
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
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
stAsText :: Field SqlGeometry -> Field SqlText
stAsText = UOp.ap1 "ST_AsText"

stArea :: Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stArea = UOp.ap2 "ST_Area"

stAzimuth :: Field SqlGeometry -> Field SqlGeometry -> Field SqlFloat8
stAzimuth = UOp.ap2 "ST_Azimuth"

stDistance :: Field SqlGeometry -> Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stDistance = UOp.ap3 "ST_Distance"

stLength :: Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stLength = UOp.ap2 "ST_Length"

stPerimeter :: Field SqlGeometry -> Field SqlBool -> Field SqlFloat8
stPerimeter = UOp.ap2 "ST_Perimeter"

stProject :: Field SqlGeometry -> Field SqlFloat8 -> Field SqlFloat8 -> Field SqlGeometry
stProject = UOp.ap3 "ST_Project"
