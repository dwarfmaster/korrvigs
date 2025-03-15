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
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections
import Options.Applicative
import Prelude hiding (putStrLn)

data AnyCol = forall mtdt. (ExtraMetadata mtdt, MtdtType mtdt ~ [[Text]]) => AnyCol mtdt

data Cmd'
  = List
      { _listPrefix :: [Text]
      }
  | Show
      { _showRec :: Bool,
        _showPrefix :: [Text]
      }

makeLenses ''Cmd'

type Cmd = (AnyCol, Cmd')

colCmd :: (ExtraMetadata mtdt, MtdtType mtdt ~ [[Text]]) => mtdt -> Text -> Mod CommandFields Cmd
colCmd mtdt mName =
  command
    (T.unpack mName)
    ( info
        (subparser (listCmd mtdt mName <> showCmd mtdt mName) <**> helper)
        ( fullDesc
            <> progDesc ("Deal with collection " <> T.unpack mName)
            <> header ("korr collections " <> T.unpack mName)
        )
    )

listCmd :: (ExtraMetadata mtdt, MtdtType mtdt ~ [[Text]]) => mtdt -> Text -> Mod CommandFields Cmd
listCmd mtdt mName =
  command
    "list"
    ( info
        ( (AnyCol mtdt,)
            <$> ( List
                    <$> many (argument str (metavar "HEADER" <> help "Header in the collection to select for"))
                )
        )
        ( progDesc ("List " <> T.unpack mName)
            <> header ("korr collections " <> T.unpack mName <> " list")
        )
    )

showCmd :: (ExtraMetadata mtdt, MtdtType mtdt ~ [[Text]]) => mtdt -> Text -> Mod CommandFields Cmd
showCmd mtdt mName =
  command
    "show"
    ( info
        ( (AnyCol mtdt,)
            <$> ( Show
                    <$> switch (long "tree" <> help "Show collections")
                    <*> many (argument str (metavar "HEADER" <> help "Header in the collection to select for"))
                )
        )
        ( progDesc ("Show " <> T.unpack mName)
            <> header ("korr collections " <> T.unpack mName <> " show")
        )
    )

parser' :: Parser Cmd
parser' =
  subparser
    (colCmd Favourite "favourite")

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
run (AnyCol col, List prefix) = do
  tree <- colCatTree col prefix
  liftIO $ displayTree False 0 False "" tree
run (AnyCol col, Show rec prefix) = do
  tree <- colTree col prefix rec
  liftIO $ displayTree True 0 False "" tree
