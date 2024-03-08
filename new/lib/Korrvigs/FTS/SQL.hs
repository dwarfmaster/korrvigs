module Korrvigs.FTS.SQL where

import Data.Profunctor.Product.Default
import Data.Text (Text)
import qualified Data.Text as T
import qualified Korrvigs.Utils.Opaleye as UOp
import Opaleye
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Opaleye.Internal.PGTypes as IPT
import qualified Opaleye.Internal.RunQuery as RQ

-- Config
data SqlRegConfig

instance IsSqlType SqlRegConfig where
  showSqlType _ = "regconfig"

tsConfigByName :: Field SqlText -> Field SqlRegConfig
tsConfigByName = C.Column . HPQ.CastExpr (showSqlType (Nothing :: Maybe SqlRegConfig)) . C.unColumn

-- TSVector
data SqlTSVector

instance IsSqlType SqlTSVector where
  showSqlType _ = "tsvector"

tsParse :: Field SqlText -> Field SqlText -> Field SqlTSVector
tsParse = UOp.ap2 "to_tsvector" . tsConfigByName

tsParseEnglish :: Field SqlText -> Field SqlTSVector
tsParseEnglish = tsParse $ sqlStrictText "english"

pgTSVector :: Text -> Field SqlTSVector
pgTSVector = IPT.literalColumn . HPQ.StringLit . T.unpack

instance Default ToFields () (Field SqlTSVector) where
  def = toToFields $ \() -> pgTSVector ""

instance DefaultFromField SqlTSVector () where
  defaultFromField = RQ.FromField . const . const . pure $ ()

-- TSQuery
-- TODO
