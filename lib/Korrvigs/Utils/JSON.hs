module Korrvigs.Utils.JSON where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as Bld
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Korrvigs.Utils.Opaleye as UOp
import Opaleye

writeJsonToFile :: (MonadIO m, ToJSON x) => FilePath -> x -> m ()
writeJsonToFile path val =
  liftIO $ LBS.writeFile path $ LEnc.encodeUtf8 $ Bld.toLazyText $ encodeToTextBuilder val

fromJSONM :: (FromJSON a) => Value -> Maybe a
fromJSONM v = case fromJSON v of
  Error _ -> Nothing
  Success x -> Just x

jsonAsText :: Value -> Maybe Text
jsonAsText (String txt) = Just txt
jsonAsText _ = Nothing

sqlJsonTypeof :: Field SqlJsonb -> Field SqlText
sqlJsonTypeof = UOp.ap1 "jsonb_typeof"

sqlTextToJson :: Field SqlText -> Field SqlJsonb
sqlTextToJson = UOp.ap1 "to_jsonb"

sqlNumToJson :: Field SqlFloat8 -> Field SqlJsonb
sqlNumToJson = UOp.ap1 "to_jsonb"

sqlJsonToText :: FieldNullable SqlJsonb -> FieldNullable SqlText
sqlJsonToText js = js .#>> sqlArray id []

sqlJsonToNum :: Field_ n SqlJsonb -> FieldNullable SqlFloat8
sqlJsonToNum = UOp.sqlCast

sqlJsonToBool :: Field_ n SqlJsonb -> FieldNullable SqlBool
sqlJsonToBool = UOp.sqlCast

sqlJsonToArray :: FieldNullable SqlJsonb -> FieldNullable (SqlArray SqlJsonb)
sqlJsonToArray = UOp.ap1 "jsonb_array_elements"
