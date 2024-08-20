module Korrvigs.Cli.Note where

import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Cli.Monad
import Korrvigs.Entry
import qualified Korrvigs.File.New as NF
import qualified Korrvigs.Link.New as NL
import Korrvigs.Monad
import Korrvigs.Note
import Korrvigs.Note.New
import Korrvigs.Note.SQL
import Opaleye
import Options.Applicative
import System.Directory (removeFile)
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn, readFile, writeFile)

data Cmd
  = Format {_formatFile :: FilePath, _inplace :: Bool}
  | New {_newNote :: NewNote}
  | Attach {_attachRef :: Text, _attachIsPath :: Bool, _attachSub :: AttachCmd}

data AttachCmd
  = AttachFiles [FilePath] Bool
  | AttachLinks [Text] Bool
  | AttachNotes [Text]

makeLenses ''Cmd

parser' :: Parser Cmd
parser' =
  subparser
    ( command
        "format"
        ( info
            ( ( Format
                  <$> argument str (metavar "FILE")
                  <*> switch (long "inplace" <> short 'i' <> help "Format FILE in place")
              )
                <**> helper
            )
            ( progDesc "Format a markdown note file"
                <> header "korr note format -- format note"
            )
        )
        <> command
          "new"
          ( info
              ( ( New
                    <$> ( NewNote
                            <$> argument str (metavar "TITLE")
                            <*> pure Nothing
                        )
                )
                  <**> helper
              )
              ( progDesc "Create a new markdown note file"
                  <> header "korr note new -- Create note"
              )
          )
        <> command
          "attach"
          ( info
              ( ( Attach
                    <$> argument str (metavar "ID")
                    <*> switch (long "path" <> short 'p' <> help "Interpret ID as a path to the markdown file instead of an id")
                    <*> parserAttach'
                )
                  <**> helper
              )
              ( progDesc "Attach entries to a note"
                  <> header "korr note attach -- Attach entries"
              )
          )
    )

parser :: ParserInfo Cmd
parser =
  info (parser' <**> helper) $
    fullDesc
      <> progDesc "Deal with note entries in Korrvigs"
      <> header "korr note -- interface for notes"

parserAttach' :: Parser AttachCmd
parserAttach' =
  subparser
    ( command
        "file"
        ( info
            ( ( AttachFiles
                  <$> many (argument str (metavar "FILE" <> help "FILE to attach"))
                  <*> switch (long "delete" <> help "Delete original files after insertion")
              )
                <**> helper
            )
            ( progDesc "Attach file(s) to a note"
                <> header "korr note attach NOTE file -- Attach files"
            )
        )
        <> command
          "link"
          ( info
              ( ( AttachLinks
                    <$> many (argument str (metavar "LINK" <> help "LINK to attach"))
                    <*> switch (long "offline" <> help "Do not download metadata from link")
                )
                  <**> helper
              )
              ( progDesc "Attach link(s) to a note"
                  <> header "korr note attach NOTE link -- Attach links"
              )
          )
        <> command
          "note"
          ( info
              ( ( AttachNotes
                    <$> many (argument str (metavar "NOTE" <> help "NOTE to attach"))
                )
                  <**> helper
              )
              ( progDesc "Attach note(s) to a note"
                  <> header "korr note attach NOTE note -- Attach notes"
              )
          )
    )

resolveId :: Text -> Bool -> KorrM (Maybe Id)
resolveId i False =
  rSelectOne $ do
    note <- selectTable notesTable
    where_ $ note ^. sqlNoteName .== sqlStrictText i
    pure $ note ^. sqlNoteName
resolveId path True =
  rSelectOne $ do
    note <- selectTable notesTable
    where_ $ note ^. sqlNotePath .== sqlStrictText path
    pure $ note ^. sqlNoteName

run :: Cmd -> KorrM ()
run (Format path inline) = do
  doRead <- readNote path
  case doRead of
    Left err -> liftIO . putStrLn $ "Could not parse " <> T.pack path <> ": " <> err
    Right doc -> do
      handle <- if inline then liftIO $ openFile path WriteMode else pure stdout
      doWrite <- writeNote handle doc
      liftIO $ hClose handle
      case doWrite of
        Just err -> liftIO . putStrLn $ "Could not write document: " <> err
        Nothing -> pure ()
run (New note) = do
  i <- new note
  liftIO $ putStrLn $ "Added note as " <> unId i
run (Attach note isPath cmd) =
  resolveId note isPath >>= \case
    Nothing -> liftIO $ putStrLn $ note <> " is not a known note id or path"
    Just i -> case cmd of
      AttachFiles files delete ->
        forM_ files $ \file -> do
          let options =
                NF.NewFile
                  { NF._nfParent = Just i,
                    NF._nfDate = Nothing,
                    NF._nfTitle = Nothing
                  }
          ni <- NF.new file options
          liftIO $ putStrLn $ unId ni
          when delete $ liftIO $ removeFile file
      AttachLinks links offline ->
        forM_ links $ \link -> do
          let options =
                NL.NewLink
                  { NL._nlOffline = offline,
                    NL._nlDate = Nothing,
                    NL._nlTitle = Nothing,
                    NL._nlParent = Just i
                  }
          ni <- NL.new link options
          liftIO $ putStrLn $ unId ni
      AttachNotes notes ->
        forM_ notes $ \nt -> do
          let options =
                NewNote
                  { _nnTitle = nt,
                    _nnParent = Just i
                  }
          ni <- new options
          liftIO $ putStrLn $ unId ni
