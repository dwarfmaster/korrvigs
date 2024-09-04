module Korrvigs.Web.Ressources.Helpers where

import Data.ByteString
import Data.Text.Encoding
import Data.Text.Internal.Builder
import System.FilePath
import Text.Julius

rcPath :: FilePath -> FilePath
rcPath file = "lib/Korrvigs/Web/Ressources" </> file

css :: FilePath -> FilePath
css file = rcPath $ "css" </> file

html :: FilePath -> FilePath
html file = rcPath $ "html" </> file

js :: FilePath -> FilePath
js file = rcPath $ "js" </> file

mkJs :: ByteString -> Javascript
mkJs = toJavascript . RawJavascript . fromText . decodeUtf8
