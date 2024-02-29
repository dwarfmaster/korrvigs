{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Geometry.SQL (SqlGeometry, sqlGeometry) where

import qualified Data.ByteString.Lazy as BL
import Data.Profunctor.Product.Default (Default, def)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeLatin1)
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Korrvigs.Geometry.Def
import Korrvigs.Geometry.WKB
import Opaleye (DefaultFromField, Field, ToFields, defaultFromField, fromPGSFromField, toToFields)
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
