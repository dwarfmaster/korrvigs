module Korrvigs.Utils where

import Data.Foldable
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

-- Execute actions stopping at the first Just. Returns Nothing if no
-- Just was found.
firstJustM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustM = foldlM go Nothing
  where
    go :: (Monad m) => Maybe a -> m (Maybe a) -> m (Maybe a)
    go Nothing action = action
    go result _ = pure result
