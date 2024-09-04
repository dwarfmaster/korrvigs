module Korrvigs.Web.Ressources.Helpers where

import System.FilePath

rcPath :: FilePath -> FilePath
rcPath file = "lib/Korrvigs/Web/Ressources" </> file

css :: FilePath -> FilePath
css file = rcPath $ "css" </> file
