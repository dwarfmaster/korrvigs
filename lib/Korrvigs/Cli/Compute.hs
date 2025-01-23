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

data CmpSelect = CmpSelect
  { _selKind :: [Kind],
    _selEntries :: [Id]
  }

data Cmd
  = Run {_runSelect :: CmpSelect, _runCmps :: [Text]}
  | List {_listEntry :: Id, _listExtract :: Bool}
  | Extract {_exSelect :: CmpSelect}

makeLenses ''CmpSelect
makeLenses ''Cmd

kindParser :: ReadM Kind
kindParser = eitherReader $ \case
  "link" -> Right Link
  "event" -> Right Event
  "file" -> Right File
  "note" -> Right Note
  s -> Left $ "\"" <> s <> "\" is not a valid kind name"

selectParser :: Parser CmpSelect
selectParser =
  CmpSelect
    <$> many (option kindParser $ metavar "KIND" <> long "kind" <> help "Kind(s) of entries to run computations on")
    <*> many (fmap MkId $ option str $ metavar "ID" <> long "entry" <> help "Entry(ies) to run computations on")

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "run"
      ( info
          ( ( Run
                <$> selectParser
                <*> many (option str $ metavar "COMP" <> long "computation" <> short 'c' <> help "Computations to run")
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
                  <*> switch (help "Extract actions before listing" <> long "extract")
              )
                <**> helper
            )
            ( progDesc "List extracted computations for entry"
                <> header "korr compute list -- list computations"
            )
        )
      <> command
        "extract"
        ( info
            ((Extract <$> selectParser) <**> helper)
            ( progDesc "Extract available computations for entry, and store them"
                <> header "korr compute extract -- extract computations"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with computations associated to entries"
      <> header "korr compute -- interface for computations"

selectEntries :: CmpSelect -> KorrM (S.Set Id)
selectEntries (CmpSelect kinds entries) = do
  kdEntries <- fmap mconcat $ forM kinds $ \kd -> fmap S.fromList $ rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind kd
    pure $ entry ^. sqlEntryName
  pure $ S.fromList entries <> kdEntries

run :: Cmd -> KorrM ()
run (Run sel comps) = do
  toRunOn <- selectEntries sel
  let idKorr :: KorrM () -> KorrM () = id
  let runAll cmps act = forM_ (M.toList cmps) $ act . snd
  let runSome cmps act =
        idKorr $ forM_ comps $ \cmpName -> maybe (pure ()) act $ M.lookup cmpName cmps
  let doRun = if null comps then runAll else runSome
  forM_ toRunOn $ \i -> do
    liftIO $ putStrLn $ "=== " <> unId i <> " ==="
    cmps <- entryStoredComputations i
    doRun cmps $ \cmp -> do
      liftIO $ putStrLn $ "> " <> cmp ^. cmpId
      Cmp.run cmp
run (List i extract) = do
  when extract $ doExtract i
  comps <- entryStoredComputations i
  forM_ (view _2 <$> M.toList comps) $ \cmp -> liftIO $ do
    putStr (cmp ^. cmpId)
    putStr " -> "
    displayAction $ cmp ^. cmpAction
    putStr " :: "
    displayType $ cmp ^. cmpType
    putStrLn ""
run (Extract sel) = do
  toRunOn <- selectEntries sel
  forM_ toRunOn $ \i -> do
    liftIO $ putStrLn $ "=== " <> unId i <> " ==="
    doExtract i

doExtract :: (MonadKorrvigs m) => Id -> m ()
doExtract i = do
  entry <- load i >>= throwMaybe (KCantLoad i "")
  comps <- listCompute entry
  syncComputations i comps

displayAction :: Action -> IO ()
displayAction (Builtin blt) = putStr "[" >> displayBuiltin blt >> putStr "]"

displayBuiltin :: Builtin.Action -> IO ()
displayBuiltin Builtin.Miniature = putStr "miniature"
displayBuiltin Builtin.Size = putStr "size"

displayType :: CompType -> IO ()
displayType ScalarImage = putStr "scalar"
displayType Picture = putStr "picture"
displayType VectorImage = putStr "vector"
displayType Json = putStr "json"
