module Korrvigs.Utils where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Foldable
import Data.Maybe
import System.Directory
import System.FilePath

resolveSymbolicLink :: FilePath -> IO FilePath
resolveSymbolicLink link =
  doesFileExist link >>= \case
    True -> do
      sym <- pathIsSymbolicLink link
      if sym
        then do
          target <- getSymbolicLinkTarget link
          let ntarget = combinePath (takeDirectory link) target
          resolveSymbolicLink ntarget
        else pure link
    False -> pure link

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

findM :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m (Maybe a)
findM check = foldlM go Nothing
  where
    go (Just v) _ = pure $ Just v
    go Nothing a =
      check a >>= \b ->
        if b
          then pure (Just a)
          else pure Nothing

partitionM :: (Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM check (x : xs) = do
  c <- check x
  (checked, unchecked) <- partitionM check xs
  pure $ if c then (x : checked, unchecked) else (checked, x : unchecked)

joinNull :: (a -> Bool) -> Maybe a -> Maybe a
joinNull f mx = do
  x <- mx
  guard $ not $ f x
  pure x

-- Remove file, then delete its parents directory if it is empty, and recursively
-- until root.
recursiveRemoveFile :: (MonadIO m) => FilePath -> FilePath -> m ()
recursiveRemoveFile root file = liftIO $ do
  ex <- doesFileExist file
  when ex $ removeFile file
  recursiveRemoveDir root $ takeDirectory file

recursiveRemoveDir :: (MonadIO m) => FilePath -> FilePath -> m ()
recursiveRemoveDir _ "/" = pure ()
recursiveRemoveDir root dir | normalise root == normalise dir = pure ()
recursiveRemoveDir root dir = liftIO $ do
  ex <- doesDirectoryExist dir
  when ex $ do
    content <- listDirectory dir
    when (null content) $ do
      removeDirectory dir
      recursiveRemoveDir root $ takeDirectory dir

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

fromMaybeT :: (Monad m) => a -> MaybeT m a -> m a
fromMaybeT d act = fromMaybe d <$> runMaybeT act
