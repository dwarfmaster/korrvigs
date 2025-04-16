module Korrvigs.Cli.Compute where

import Control.Arrow ((&&&))
import Control.Lens hiding (List, argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStr, putStrLn)
import Korrvigs.Actions.Metadata
import Korrvigs.Cli.Monad
import Korrvigs.Compute
import qualified Korrvigs.Compute as Cmp
import Korrvigs.Compute.Action
import qualified Korrvigs.Compute.Builtin as Builtin
import Korrvigs.Compute.Declare
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Utils.Crypto
import Opaleye hiding (null)
import Options.Applicative
import System.IO (stdin)
import Prelude hiding (putStr, putStrLn)

data CmpSelect = CmpSelect
  { _selKind :: [Kind],
    _selEntries :: [Id]
  }

data Cmd
  = Run {_runSelect :: CmpSelect, _runCmps :: [Text], _runLazy :: Bool}
  | List {_listEntry :: Id}
  | Store

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
        "store"
        ( info
            ( pure Store <**> helper
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

selectEntries :: CmpSelect -> KorrM (S.Set Id)
selectEntries (CmpSelect kinds entries) = do
  kdEntries <- fmap mconcat $ forM kinds $ \kd -> fmap S.fromList $ rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind kd
    pure $ entry ^. sqlEntryName
  pure $ S.fromList entries <> kdEntries

run :: Cmd -> KorrM ()
run (Run sel comps lz) = do
  toRunOn <- selectEntries sel
  let mcomps = M.fromList $ (id &&& const ()) <$> comps
  let select cmps = if null comps then cmps else M.intersectionWith (const id) mcomps cmps
  forM_ toRunOn $ \i -> do
    liftIO $ putStrLn $ "=== " <> unId i <> " ==="
    cmps <- select <$> listCompute i
    forM_ (M.toList cmps) $ \(nm, cmp) -> do
      liftIO $ putStrLn $ "> " <> nm
      file <- (if lz then Cmp.lazyRun else Cmp.register) i nm cmp
      liftIO $ putStrLn $ T.pack file
run (List i) = do
  comps <- listCompute i
  forM_ (M.toList comps) $ \(nm, act) -> liftIO $ do
    putStr nm
    putStr " -> "
    displayAction act
run Store = do
  input <- liftIO $ LBS.hGetContents stdin
  case eitherDecode input of
    Left err -> liftIO $ putStrLn $ "Failed to parse json: " <> T.pack (show err)
    Right (v :: Value) -> do
      (hash, _) <- storeCachedJson v
      liftIO $ putStrLn $ digestToHexa hash

displayAction :: Action -> IO ()
displayAction (Builtin _ blt) = putStr "[" >> displayBuiltin blt >> putStr "]"
displayAction (Cached tp _) = putStr "{" >> displayType tp >> putStr "}"

displayBuiltin :: Builtin.Action -> IO ()
displayBuiltin Builtin.Miniature = putStr "miniature"
displayBuiltin Builtin.Size = putStr "size"

displayType :: CompType -> IO ()
displayType ScalarImage = putStr "scalar"
displayType Picture = putStr "picture"
displayType VectorImage = putStr "vector"
displayType Json = putStr "json"
