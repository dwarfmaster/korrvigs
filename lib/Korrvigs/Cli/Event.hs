module Korrvigs.Cli.Event where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Text.IO (putStrLn)
import Korrvigs.Actions (processRelData)
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Event.Sync
import Options.Applicative
import Prelude hiding (putStrLn)

data Cmd
  = Register {_regSilent :: Bool, _regJSON :: Bool}
  | Pull
  | Push

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "register"
      ( info
          ( ( Register
                <$> switch (long "silent" <> help "Cancel informational output")
                <*> switch (long "json" <> help "Display information as json")
            )
              <**> helper
          )
          ( progDesc "Register new events"
              <> header "korr event register -- register events"
          )
      )
      <> command
        "pull"
        ( info
            (pure Pull <**> helper)
            ( progDesc "Pull events in separate worktree"
                <> header "korr event pull -- pull events"
            )
        )
      <> command
        "push"
        ( info
            (pure Push <**> helper)
            ( progDesc "Push events from main worktree"
                <> header "korr event push -- push events"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with event entries in Korrvigs"
      <> header "korr event -- interface for events"

run :: Cmd -> KorrM ()
run (Register silent json) = do
  evs <- allEvents
  forM_ evs $ \(cal, ics) -> do
    (i, ical, ievent, new) <- register (cal, ics)
    when new $ do
      syncOneEvent i cal ics ical ievent >>= processRelData i
      unless silent $
        liftIO $
          if json
            then putStrLn $ "\"" <> unId i <> "\""
            else putStrLn $ "Registered " <> cal <> "/" <> ics <> " as " <> unId i
run Pull = undefined
run Push = undefined
