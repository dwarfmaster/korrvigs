module Korrvigs.Cli.Remove where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Monad.Remove
import Korrvigs.Monad.SQL
import Options.Applicative

newtype Cmd = Remove {_rmEntries :: [Id]}

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  Remove <$> many (MkId <$> argument str (help "Entry to remove by id" <> metavar "ID"))

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Remove entries"
      <> header "korr remove -- remove entries"

run :: Cmd -> KorrM ()
run torm = forM_ (torm ^. rmEntries) $ \i ->
  load i >>= \case
    Nothing -> liftIO $ putStrLn $ T.unpack (unId i) <> " is not an entry"
    Just entry -> do
      liftIO $ putStrLn $ "Removing " <> T.unpack (unId i)
      removeDWIM entry
