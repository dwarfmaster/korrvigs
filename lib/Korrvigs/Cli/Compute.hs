module Korrvigs.Cli.Compute where

import Control.Lens hiding (List, argument)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import Data.Text.IO (putStr, putStrLn)
import Korrvigs.Actions.Compute
import Korrvigs.Cli.Monad
import Korrvigs.Compute hiding (run)
import qualified Korrvigs.Compute as Cmp
import qualified Korrvigs.Compute.Builtin as Builtin
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye hiding (null)
import Options.Applicative
import Prelude hiding (putStr, putStrLn)

data Cmd
  = Run {_runKind :: [Kind], _runEntries :: [Id], _runCmps :: [Text]}
  | List {_listEntry :: Id}

makeLenses ''Cmd

kindParser :: ReadM Kind
kindParser = eitherReader $ \case
  "link" -> Right Link
  "event" -> Right Event
  "file" -> Right File
  "note" -> Right Note
  s -> Left $ "\"" <> s <> "\" is not a valid kind name"

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "run"
      ( info
          ( ( Run
                <$> many (option kindParser $ metavar "KIND" <> long "kind" <> help "Kind(s) of entries to run computations on")
                <*> many (fmap MkId $ option str $ metavar "ID" <> long "entry" <> help "Entry(ies) to run computations on")
                <*> many (option str $ metavar "COMP" <> long "computation" <> short 'c' <> help "Computations to run")
            )
              <**> helper
          )
          ( progDesc "Run computations associated to entries"
              <> header "korr compute run -- run computation"
          )
      )
      <> command
        "list"
        ( info
            ((List . MkId <$> argument str (metavar "ID" <> help "Entry to list available computations for")) <**> helper)
            ( progDesc "List available computations for entry"
                <> header "korr compute list -- list computations"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with computations associated to entries"
      <> header "korr compute -- interface for computations"

run :: Cmd -> KorrM ()
run (Run kinds entries comps) = do
  kdEntries <- fmap mconcat $ forM kinds $ \kd -> fmap S.fromList $ rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind kd
    pure $ entry ^. sqlEntryName
  let toRunOn = S.fromList entries <> kdEntries
  let idKorr :: KorrM () -> KorrM () = id
  let runAll cmps act = forM_ (M.toList cmps) $ act . snd
  let runSome cmps act =
        idKorr $ forM_ comps $ \cmpName -> maybe (pure ()) act $ M.lookup cmpName cmps
  let doRun = if null comps then runAll else runSome
  forM_ toRunOn $ \i -> do
    liftIO $ putStrLn $ "=== " <> unId i <> " ==="
    entry <- load i >>= throwMaybe (KCantLoad i "")
    cmps <- listCompute entry
    storeComputations i cmps
    doRun cmps $ \cmp -> do
      liftIO $ putStrLn $ "> " <> cmp ^. cmpId
      Cmp.run cmp
run (List i) = do
  entry <- load i >>= throwMaybe (KCantLoad i "")
  comps <- listCompute entry
  storeComputations i comps
  forM_ (view _2 <$> M.toList comps) $ \cmp -> liftIO $ do
    putStr (cmp ^. cmpId)
    putStr " -> "
    displayAction $ cmp ^. cmpAction
    putStr " :: "
    displayType $ cmp ^. cmpType
    putStrLn ""

displayAction :: Action -> IO ()
displayAction (Builtin blt) = putStr "[" >> displayBuiltin blt >> putStr "]"

displayBuiltin :: Builtin.Action -> IO ()
displayBuiltin Builtin.Miniature = putStr "miniature"

displayType :: CompType -> IO ()
displayType ScalarImage = putStr "scalar"
displayType Picture = putStr "picture"
displayType VectorImage = putStr "vector"
