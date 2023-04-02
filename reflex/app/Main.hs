{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Reflex
import Reflex.Host.Headless

setupApp :: MonadHeadlessApp t m => m (Event t ())
setupApp = do
  (inputEvent, inputTrigger) <- newTriggerEvent
  _ <- liftIO . forkIO . forever $ getLine >>= inputTrigger
  performEvent_ $ liftIO . putStrLn <$> inputEvent
  return $ () <$ ffilter (\str -> str == "quit") inputEvent

main :: IO ()
main = runHeadlessApp setupApp
