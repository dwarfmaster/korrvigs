module Korrvigs.Utils where

import System.Directory
import System.FilePath

resolveSymbolicLink :: FilePath -> IO FilePath
resolveSymbolicLink link = do
  sym <- pathIsSymbolicLink link
  if sym
    then do
      target <- getSymbolicLinkTarget link
      let ntarget = combinePath (takeDirectory link) target
      resolveSymbolicLink ntarget
    else pure link

combinePath :: FilePath -> FilePath -> FilePath
combinePath rt pth
  | isAbsolute pth = pth
  | otherwise = rt </> pth
