{-# OPTIONS_GHC -fno-warn-orphans #-}

module Korrvigs.Link where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (readFile)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import System.FilePath
import Prelude hiding (readFile)

instance IsKD Link where
  data KDMaker Link = LinkMaker
  dLoad = undefined
  dMake = undefined
  dList = undefined
  dSync = undefined
  dKind = const Link
  dEntry = _linkEntry

-- Load link from database
loadLink :: MonadKorrvigs m => Id -> m (Maybe Link)
loadLink = undefined

-- Add new link to file and database
-- Its ID must not be present in the database
addLink :: MonadKorrvigs m => Link -> m ()
addLink = undefined

-- Load all links from the filesystem
loadLinks :: MonadKorrvigs m => m (Either Text [Link])
loadLinks = do
  undefined

-- rt <- root
-- let path = joinPath [rt, "links.json"]
-- content <- liftIO $ readFile path
-- pure $ case eitherDecode content of
--   Right links -> Right $ buildLink <$> links
--   Left err -> Left $ T.pack err
