module Korrvigs.Cli.Compute where

import Control.Lens hiding (List, argument)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import Data.Text.IO (putStr, putStrLn)
import Korrvigs.Cli.Monad
import qualified Korrvigs.Compute.Run as Run
import Korrvigs.Compute.SQL
import Korrvigs.Compute.Type (encodeToLBS)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Monad.Computation
import Korrvigs.Utils.Crypto
import Opaleye hiding (null)
import Options.Applicative
import Prelude hiding (putStr, putStrLn)

data Cmd
  = Run {_runEntry :: Id, _runCmp :: Text, _runLazy :: Bool}
  | List {_listEntry :: Id}
  | Hash {_hashEntry :: Id, _hashCmp :: Text}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "run"
      ( info
          ( ( Run . MkId
                <$> argument str (metavar "ENTRY")
                <*> argument str (metavar "COMP")
                <*> switch (long "lazy" <> help "Do not run if computation has already been computed (even if it is out of date)")
            )
              <**> helper
          )
          ( progDesc "Run extracted computations associated to entries"
              <> header "korr compute run -- run computation"
          )
      )
      <> command
        "list"
        ( info
            ( ( List
                  <$> fmap MkId (argument str (metavar "ID" <> help "Entry to list available computations for"))
              )
                <**> helper
            )
            ( progDesc "List extracted computations for entry"
                <> header "korr compute list -- list computations"
            )
        )
      <> command
        "hash"
        ( info
            ( ( Hash . MkId
                  <$> argument str (metavar "ENTRY" <> help "Entry to hash computation for")
                  <*> argument str (metavar "COMP" <> help "Computation to hash in the selected entry")
              )
                <**> helper
            )
            ( progDesc "Store new cached json data into database"
                <> header "korr compute store -- store json"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with computations associated to entries"
      <> header "korr compute -- interface for computations"

run :: Cmd -> KorrM ()
run (Run i cmp lz) =
  getComputation i cmp >>= \case
    Nothing -> liftIO $ putStrLn "Failed to load computation"
    Just comp ->
      runner comp >>= \case
        Left err -> liftIO $ putStrLn $ "Computation failed with " <> err
        Right res -> liftIO $ LBS.putStr $ encodeToLBS res
  where
    runner = if lz then Run.runLazy else Run.runForce
run (List i) = do
  comps <- rSelect $ do
    cmp <- selectTable computationsTable
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== (cmp ^. sqlCompEntry)
    where_ $ entry ^. sqlEntryName .== sqlId i
    pure $ cmp ^. sqlCompName
  forM_ comps $ liftIO . putStr
run (Hash i cmp) =
  getComputation i cmp >>= \case
    Nothing -> liftIO $ putStrLn "Failed to load computation"
    Just comp ->
      Run.hashComputation comp
        >>= liftIO . \case
          Nothing -> putStrLn "Failed to hash computation"
          Just hash -> putStrLn $ digestToText hash
