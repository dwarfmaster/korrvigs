{-# LANGUAGE ExistentialQuantification #-}

module Korrvigs.Cli.Collections where

import Control.Lens hiding (List, argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import Korrvigs.Metadata.Collections
import Options.Applicative
import Prelude hiding (putStrLn)

data Cmd
  = List
      { _listPrefix :: [Text]
      }
  | Show
      { _showRec :: Bool,
        _showPrefix :: [Text]
      }

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser $
    command
      "list"
      ( info
          ( List
              <$> many (argument str (metavar "HEADER" <> help "Header in the collection to select for"))
          )
          ( progDesc "List elements"
              <> header "korr collections list"
          )
      )
      <> command
        "show"
        ( info
            ( Show
                <$> switch (long "tree" <> help "Show collections")
                <*> many (argument str (metavar "HEADER" <> help "Header in the collection to select for"))
            )
            ( progDesc "Show elements"
                <> header "korr collections show"
            )
        )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "List entries in collections"
      <> header "korr collections -- list collections"

displayTree :: Bool -> Int -> Bool -> Text -> ColTree -> IO ()
displayTree showEntries prefixL isFirst hd tree = do
  let prefixHd =
        if isFirst && prefixL > 1
          then T.replicate (prefixL - 2) "| " <> "+-"
          else T.replicate (prefixL - 1) "| "
  unless (T.null hd) $ putStrLn $ prefixHd <> "+> " <> hd
  when showEntries $ do
    forM_ (tree ^. colEntries) $ \(MkId i, mtitle) -> case mtitle of
      Just title -> putStrLn $ prefix <> title <> " (@" <> i <> ")"
      Nothing -> putStrLn $ prefix <> "@" <> i
  isFst <- newIORef True
  forM_ (M.toList $ tree ^. colSubs) $ \(subHd, sub) -> do
    subFst <- readIORef isFst
    displayTree showEntries (prefixL + 1) subFst subHd sub
    writeIORef isFst False
  where
    prefix = T.replicate prefixL "| "

run :: Cmd -> KorrM ()
run (List prefix) = do
  tree <- colCatTree MiscCollection prefix
  liftIO $ displayTree False 0 False "" tree
run (Show rec prefix) = do
  tree <- colTree MiscCollection prefix rec
  liftIO $ displayTree True 0 False "" tree
