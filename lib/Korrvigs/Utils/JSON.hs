module Korrvigs.Utils.JSON where

import Data.Aeson
import Data.Text (Text)
import qualified Korrvigs.Utils.Opaleye as UOp
import Opaleye

jsonAsText :: Value -> Maybe Text
jsonAsText (String txt) = Just txt
jsonAsText _ = Nothing

sqlJsonTypeof :: Field SqlJsonb -> Field SqlText
sqlJsonTypeof = UOp.ap1 "jsonb_typeof"

sqlTextToJson :: Field SqlText -> Field SqlJsonb
sqlTextToJson = UOp.ap1 "to_jsonb"

sqlNumToJson :: Field SqlFloat8 -> Field SqlJsonb
sqlNumToJson = UOp.ap1 "to_jsonb"

sqlJsonToText :: Field_ n SqlJsonb -> FieldNullable SqlText
sqlJsonToText = UOp.sqlCast

sqlJsonToNum :: Field_ n SqlJsonb -> FieldNullable SqlFloat8
sqlJsonToNum = UOp.sqlCast

sqlJsonToBool :: Field_ n SqlJsonb -> FieldNullable SqlBool
sqlJsonToBool = UOp.sqlCast

sqlJsonToArray :: FieldNullable SqlJsonb -> FieldNullable (SqlArray SqlJsonb)
sqlJsonToArray = UOp.ap1 "jsonb_array_elements"
