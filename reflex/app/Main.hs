{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Host.Headless
import qualified System.FSNotify as FS
import System.FilePath.Posix (takeBaseName)

setupWatch :: MonadHeadlessApp t m => m (Event t FS.Event)
setupWatch = do
  (fsEvent, fsTrigger) <- newTriggerEvent
  _ <- liftIO . forkIO . FS.withManager $ \mgr -> do
    _ <- FS.watchTree mgr "test" (const True) fsTrigger
    forever $ threadDelay 1000000
  return fsEvent

setupApp :: MonadHeadlessApp t m => m (Event t ())
setupApp = do
  watch <- setupWatch
  performEvent_ $ liftIO . putStrLn . show <$> watch
  return $ () <$ ffilter (\ev -> takeBaseName (FS.eventPath ev) == "quit") watch

main :: IO ()
main = runHeadlessApp setupApp
