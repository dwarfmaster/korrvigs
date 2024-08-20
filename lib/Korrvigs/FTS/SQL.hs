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

tsConfigEnglish :: Field SqlRegConfig
tsConfigEnglish = tsConfigByName $ sqlStrictText "english"

-- TSVector
data SqlTSVector

instance IsSqlType SqlTSVector where
  showSqlType _ = "tsvector"

tsParse :: Field SqlRegConfig -> Field SqlText -> Field SqlTSVector
tsParse = UOp.ap2 "to_tsvector"

tsParseEnglish :: Field SqlText -> Field SqlTSVector
tsParseEnglish = tsParse tsConfigEnglish

pgTSVector :: Text -> Field SqlTSVector
pgTSVector = IPT.literalColumn . HPQ.StringLit . T.unpack

instance Default ToFields () (Field SqlTSVector) where
  def = toToFields $ \() -> pgTSVector ""

instance DefaultFromField SqlTSVector () where
  defaultFromField = RQ.FromField . const . const . pure $ ()

-- TSQuery
data SqlTSQuery

instance IsSqlType SqlTSQuery where
  showSqlType _ = "tsquery"

data TSQuery
  = TSText Text
  | TSAnd TSQuery TSQuery
  | TSOr TSQuery TSQuery
  | TSNot TSQuery
  | TSSeq TSQuery TSQuery
  deriving (Eq, Show)

pgQuery :: TSQuery -> Field SqlTSQuery
pgQuery (TSText txt) = UOp.ap2 "to_tsquery" tsConfigEnglish $ sqlStrictText txt
pgQuery (TSAnd ts1 ts2) = C.binOp (HPQ.OpOther "&&") (pgQuery ts1) (pgQuery ts2)
pgQuery (TSOr ts1 ts2) = C.binOp (HPQ.OpOther "||") (pgQuery ts1) (pgQuery ts2)
pgQuery (TSNot ts) = C.unOp (HPQ.UnOpOther "!!") $ pgQuery ts
pgQuery (TSSeq ts1 ts2) = C.binOp (HPQ.OpOther "<->") (pgQuery ts1) (pgQuery ts2)

infix 4 @@

(@@) :: Field SqlTSQuery -> Field SqlTSVector -> Field SqlBool
(@@) = C.binOp (HPQ.OpOther "@@")
