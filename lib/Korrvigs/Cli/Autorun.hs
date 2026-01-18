module Korrvigs.Cli.Autorun where

import Control.Lens hiding (List, argument, ignored)
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Calendar hiding (periodLength)
import Data.Time.Format.ISO8601
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Monad.Metadata.Autorun
import Options.Applicative

data Cmd
  = List {_listTime :: Maybe Int}
  | Run {_runTime :: Int}

makePrisms ''Cmd
makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "list"
      ( info
          ((List <$> optional (option auto (metavar "MS" <> long "time" <> help "Combined runtime of targets to list"))) <**> helper)
          ( progDesc "List next targets to autorun"
              <> header "korr autorun list -- list targets to autorun"
          )
      )
      <> command
        "run"
        ( info
            ((Run <$> argument auto (metavar "MS" <> help "Combined runtime of targets to run")) <**> helper)
            ( progDesc "Run targets until time is elapsed"
                <> header "korr autorun run -- run targets"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Autorun some targets"
      <> header "korr autorun -- autorun targets"

displayTarget :: AutoRunnableTarget -> Text
displayTarget (AutoSyn syn) = "syn:" <> unId (syn ^. synEntry . entryName)

displayPeriod :: RunPeriod -> Text
displayPeriod period =
  mconcat
    [ if m /= 0 then T.pack (show m) <> "m" else "",
      if d /= 0 then T.pack (show d) <> "d" else "",
      maybe "" dayT (period ^. periodAlign)
    ]
  where
    m = period ^. periodLength . to cdMonths
    d = period ^. periodLength . to cdDays
    dayT Monday = " mon"
    dayT Tuesday = " tue"
    dayT Wednesday = " wed"
    dayT Thursday = " thu"
    dayT Friday = " fri"
    dayT Saturday = " sat"
    dayT Sunday = " sun"

displayAuto :: AutoRunnable -> Text
displayAuto autoRun =
  T.intercalate " " $
    filter
      (not . T.null)
      [ displayTarget (autoRun ^. autoTarget),
        displayPeriod (autoRun ^. autoPeriod),
        maybe "" (("last-run:" <>) . T.pack . iso8601Show) (autoRun ^. autoLastRun),
        maybe "" (("runtime:" <>) . T.pack . show) (autoRun ^. autoRunTime)
      ]

run :: Cmd -> KorrM ()
run (List time) = do
  targets <- targetsToRun
  forM_ targets $ \tgt -> do
    liftIO $ TIO.putStrLn $ displayAuto tgt
run (Run time) = undefined
