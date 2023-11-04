module Korrvigs.Tree where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text, unpack)
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified System.Directory as Dir
import System.FilePath ((</>))

-- Returns Right True if the directory already existed. On failure, if it
-- didn't exists, it is removed.
createEntry :: MonadIO m => FilePath -> UUID -> Text -> m Bool
createEntry root uuid md = liftIO $ do
  dirExists <- Dir.doesDirectoryExist dir
  Dir.createDirectoryIfMissing True dir
  exists <- Dir.doesFileExist mdPath
  if exists
    then pure ()
    else writeFile mdPath ""
  pure dirExists
  where
    dir :: FilePath
    dir = root </> U.toString uuid
    mdPath :: FilePath
    mdPath = dir </> unpack md
