module Korrvigs.Cli.Note where

import Conduit (throwM)
import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Entry.New
import qualified Korrvigs.File.New as NF
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note
import Korrvigs.Note.AST (bkCollection)
import Korrvigs.Note.New
import Korrvigs.Note.SQL
import Opaleye hiding (optional)
import Options.Applicative
import System.Exit
import System.IO hiding (putStrLn)
import Prelude hiding (putStrLn, readFile, writeFile)

data Cmd
  = Format {_formatFile :: FilePath, _inplace :: Bool}
  | Reformat
  | New {_newNote :: NewNote}
  | Attach {_attachRef :: Text, _attachIsPath :: Bool, _attachSub :: AttachCmd}
  | Col {_colNote :: Maybe Text, _colName :: Maybe Text, _colInsert :: Maybe Text}

data AttachCmd
  = AttachFiles [FilePath] Bool
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
          "reformat"
          ( info
              (pure Reformat <**> helper)
              ( progDesc "Reformat all notes"
                  <> header "korr note reformat -- reformat notes"
              )
          )
        <> command
          "new"
          ( info
              ( ( New
                    <$> ( NewNote
                            <$> newEntryOptions
                            <*> argument str (metavar "TITLE")
                            <*> switch (long "allow-override" <> help "If a url is present, use the title extracted from this URL instead")
                            <*> pure False
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
        <> command
          "col"
          ( info
              ( ( Col
                    <$> optional (argument str (metavar "NOTE"))
                    <*> optional (argument str (metavar "COL"))
                    <*> optional (option str (long "insert" <> help "Insert new ID into collection"))
                )
                  <**> helper
              )
              ( progDesc "Deal with note collections"
                  <> header "korr note col -- Deal with collections"
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
    nm <- nameFor $ note ^. sqlNoteId
    where_ $ nm .== sqlStrictText i
    pure nm
resolveId path True =
  rSelectOne $ do
    note <- selectTable notesTable
    where_ $ note ^. sqlNotePath .== sqlStrictText path
    nameFor $ note ^. sqlNoteId

formatNote :: FilePath -> Bool -> KorrM ()
formatNote path inline = do
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

run :: Cmd -> KorrM ()
run (Format path inline) = formatNote path inline
run Reformat = do
  paths <- rSelect $ do
    note <- selectTable notesTable
    pure $ note ^. sqlNotePath
  forM_ paths $ \path -> do
    liftIO $ putStrLn $ "Formatting " <> T.pack path
    formatNote path True
run (New note) = do
  i <- new note
  liftIO $ putStrLn $ unId i
run (Attach note isPath cmd) =
  resolveId note isPath >>= \case
    Nothing -> liftIO $ putStrLn $ note <> " is not a known note id or path"
    Just i -> case cmd of
      AttachFiles files delete ->
        forM_ files $ \file -> do
          let options =
                NF.NewFile
                  { NF._nfEntry = def & neParents .~ [i],
                    NF._nfRemove = delete
                  }
          ni <- NF.new file options
          liftIO $ putStrLn $ unId ni
      AttachNotes notes ->
        forM_ notes $ \nt -> do
          let options =
                NewNote
                  { _nnTitle = nt,
                    _nnEntry = def & neParents .~ [i],
                    _nnTitleOverride = False,
                    _nnIgnoreUrl = False
                  }
          ni <- new options
          liftIO $ putStrLn $ unId ni
run (Col Nothing _ _) = do
  cols <- allCollections
  forM_ cols $ \(MkId i, col) -> liftIO $ putStrLn $ i <> "#" <> col
run (Col (Just note) Nothing _) = do
  cols <- collectionsFor $ MkId note
  forM_ cols $ liftIO . putStrLn
run (Col (Just note) (Just nm) Nothing) =
  load (MkId note) >>= \case
    Nothing -> throwM $ KMiscError $ note <> " is not a valid entry"
    Just entry -> case entry ^. entryKindData of
      NoteD ne ->
        readNote (ne ^. notePath) >>= \case
          Left err -> throwM $ KMiscError $ "Failed to load " <> T.pack (ne ^. notePath) <> ": " <> err
          Right md -> case md ^? docContent . each . bkCollection nm of
            Just (c, _, items) -> do
              res <- loadCollection c items
              forM_ res $ liftIO . putStrLn . unId . view (_1 . sqlEntryName)
            Nothing -> throwM $ KMiscError $ note <> " note has no collection with name " <> nm
      _ -> throwM $ KMiscError $ note <> " is not a note entry"
run (Col (Just note) mnm (Just toinsert)) = do
  colNm <- throwMaybe (KMiscError "Cannot insert if the name of the collection is not specified") mnm
  r <- addToCollection (MkId note) colNm $ ColItemEntry $ MkId toinsert
  if r
    then liftIO $ putStrLn "Inclusion succeeded"
    else liftIO $ do
      putStrLn "Inclusion failed"
      exitFailure
