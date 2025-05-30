module Korrvigs.Cli.Note where

import Conduit (throwM)
import Control.Concurrent (threadDelay)
import Control.Lens hiding (argument)
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (putStrLn)
import Korrvigs.Cli.Monad
import Korrvigs.Cli.New
import Korrvigs.Entry
import Korrvigs.Entry.New
import qualified Korrvigs.File.New as NF
import qualified Korrvigs.Link.New as NL
import Korrvigs.Metadata
import Korrvigs.Metadata.Collections
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Monad.Metadata
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
  | New {_newNote :: NewNote}
  | Attach {_attachRef :: Text, _attachIsPath :: Bool, _attachSub :: AttachCmd}
  | Col {_colNote :: Maybe Text, _colName :: Maybe Text, _colInsert :: Maybe Text}
  | MigrateCol

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
                            <$> newEntryOptions
                            <*> argument str (metavar "TITLE")
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
        <> command
          "migrate"
          ( info
              (pure MigrateCol <**> helper)
              (progDesc "Migrate old collections to new system" <> header "korr note migrate")
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
      AttachLinks links offline ->
        forM_ links $ \link -> do
          let options =
                def
                  & NL.nlOffline .~ offline
                  & NL.nlEntry . neParents .~ [i]
          ni <- NL.new link options
          liftIO $ putStrLn $ unId ni
      AttachNotes notes ->
        forM_ notes $ \nt -> do
          let options =
                NewNote
                  { _nnTitle = nt,
                    _nnEntry = def & neParents .~ [i]
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
    Just entry -> case entry ^. kindData of
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
run MigrateCol = do
  let guide :: Map [Text] (Text, Text) =
        M.fromList
          [ (["Captured"], ("Favourites", "captured")),
            (["Devices", "Phones"], ("Setup", "devices")),
            (["Favourite"], ("Favourites", "main")),
            (["Favourite", "Activités"], ("Favourites", "activities")),
            (["Favourite", "Clearsy"], ("Favourites", "clearsy")),
            (["Favourite", "Clearsy", "Alstom"], ("Clearsy:1", "alstom")),
            (["Favourite", "Clearsy", "Misc"], ("Clearsy:1", "misc")),
            (["Favourite", "Clearsy", "SNCF"], ("Clearsy:1", "sncf")),
            (["Genre", "Fantasy"], ("Genres", "fantasy-pictures")),
            (["Genre", "Science fiction"], ("Genres", "sf-pictures")),
            (["Genre", "Meme"], ("Memes", "memes")),
            (["Hobbies", "Electronics"], ("Activites", "electronics-pictures")),
            (["Hobbies", "Games"], ("Activites", "games-pictures")),
            (["Hobbies", "Lego"], ("Activites", "lego-pictures")),
            (["Sport", "Bikepacking"], ("Activites", "bikepacking")),
            (["Sport", "Parkour"], ("Activites", "parkour-pictures")),
            (["Sport", "Parkour", "Bercy"], ("Activites", "parkour-pictures")),
            (["Sport", "Parkour", "Tour Essor"], ("Activites", "parkour-pictures")),
            (["Sport", "Tree climbing"], ("Activites", "tree-pictures")),
            (["Places", "Earthporn"], ("Activites", "earthporn")),
            (["Places", "Home"], ("Activites", "home-pictures")),
            (["Places", "Nans"], ("Activites", "nans-pictures")),
            (["Places", "Nans", "Crèche"], ("Diy", "creche"))
          ]
  entries <- rSelect $ do
    entry <- selectTable entriesTable
    mtdt <- selectTable entriesMetadataTable
    where_ $ mtdt ^. sqlEntry .== (entry ^. sqlEntryName)
    where_ $ mtdt ^. sqlKey .== sqlStrictText (mtdtSqlName MiscCollection)
    pure $ entry ^. sqlEntryName
  forM_ entries $ \i ->
    load i >>= \case
      Nothing -> liftIO $ putStrLn $ "Failed to load " <> unId i
      Just entry -> do
        cols <- fromMaybe [] <$> rSelectMtdt MiscCollection (sqlId i)
        forM_ cols $ \col -> do
          let (nColId, nColNm) = fromJust $ M.lookup col guide
          r <- addToCollection (MkId nColId) nColNm $ ColItemEntry i
          liftIO $ threadDelay 500000
          liftIO $ putStrLn $ unId i <> ": " <> T.pack (show col) <> " -> " <> nColId <> "#" <> nColNm <> " :: " <> T.pack (show r)
        updateMetadata entry M.empty [mtdtSqlName MiscCollection]
