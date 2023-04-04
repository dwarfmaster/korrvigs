module Main where

import Control.Arrow ((***))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Reflex
import Reflex.Host.Headless
import qualified System.FSNotify as FS
import System.FilePath.Posix (takeFileName)
import Text.Regex.Base (makeRegex, matchTest)
import Text.Regex.Posix (Regex)

patterns :: [(Regex, String)]
patterns =
  (makeRegex *** id)
    <$> [ ("\\.norg$", "Wiki file!")
        ]

doPattern :: String -> Maybe String
doPattern str = listToMaybe . map snd . flip filter patterns $ \(reg, _) -> matchTest reg str

setupWatch :: MonadHeadlessApp t m => m (Event t FS.Event)
setupWatch = do
  (fsEvent, fsTrigger) <- newTriggerEvent
  _ <- liftIO . forkIO . FS.withManager $ \mgr -> do
    _ <- FS.watchTree mgr "test" (const True) fsTrigger
    forever $ threadDelay 1000000
  return fsEvent

matchEvent :: FS.Event -> Maybe String
matchEvent (FS.Added eventPath _ _) = doPattern (takeFileName eventPath)
matchEvent (FS.Modified eventPath _ _) = doPattern (takeFileName eventPath)
matchEvent _ = Nothing

setupApp :: MonadHeadlessApp t m => m (Event t ())
setupApp = do
  watch <- setupWatch
  let pat = fmapMaybe matchEvent watch
  performEvent_ $ liftIO . putStrLn . show <$> watch
  performEvent_ $ liftIO . putStrLn . ("Pattern: " ++) <$> pat
  return $ () <$ ffilter (\ev -> takeFileName (FS.eventPath ev) == "quit") watch

main :: IO ()
main = runHeadlessApp setupApp
