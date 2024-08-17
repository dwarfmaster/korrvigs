module Korrvigs.Utils.JSON where

import Data.Aeson
import Data.Text (Text)

jsonAsText :: Value -> Maybe Text
jsonAsText (String txt) = Just txt
jsonAsText _ = Nothing
