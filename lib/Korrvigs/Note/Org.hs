module Korrvigs.Note.Org (importOrgFile) where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Foldable.Extra (firstJustM)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Korrvigs.Entry
import qualified Korrvigs.Entry.New as NE
import qualified Korrvigs.File.New as NF
import qualified Korrvigs.Kind as Kd
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.TH
import Korrvigs.Metadata.Task
import Korrvigs.Monad
import Korrvigs.Monad.Sync (syncFileOfKind)
import qualified Korrvigs.Note.AST as A
import Korrvigs.Note.Helpers (renderInlines)
import qualified Korrvigs.Note.New as NN
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Sync (updateImpl)
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Multiplate
import Korrvigs.Utils.Pandoc
import Opaleye hiding (not, optional)
import System.Directory
import Text.Pandoc
import qualified Text.Pandoc.Walk as W
import Text.Parsec hiding (try)
import qualified Text.Parsec as P
import Text.Parsec.Number

mkMtdt "RoamRefs" "roam-refs" [t|[Text]|]

importOrgFile :: (MonadKorrvigs m) => NE.NewEntry -> FilePath -> m Id
importOrgFile ne org = $withLogContext ("Importing org file \"" <> T.pack org <> "\"") $ do
  contentE <- liftIO $ (try $ TIO.readFile org :: IO (Either SomeException Text))
  content <- throwEither (\e -> KMiscError . T.pack $ "IO error: " <> show e) contentE
  fileE <- liftIO $ runIO $ readOrg def $ "#+OPTIONS: H:9\n" <> content
  file@(Pandoc meta _) <- throwEither (\e -> KMiscError . T.pack $ "Pandoc error: " <> show e) fileE
  titleV <- throwMaybe (KMiscError "Org note has no title") $ M.lookup "title" $ unMeta meta
  let title = metaAsText titleV
  let url = fmap (toJSON . metaAsText) $ M.lookup "roam_refs" $ unMeta meta
  let nnote =
        NN.NewNote
          { NN._nnEntry = ne & maybe id (NE.neMtdt . at (mtdtName Url) ?~) url,
            NN._nnTitle = title,
            NN._nnTitleOverride = False,
            NN._nnIgnoreUrl = True,
            NN._nnAllowDuplicateTitle = True
          }
  i <- NN.new nnote
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load newly created note")
  note <- throwMaybe (KMiscError $ "Note " <> unId i <> " is not a note") $ entry ^? _Note
  let orgId = maybe mempty (S.singleton . metaAsText) $ M.lookup "id" $ unMeta meta
  let orgIds = orgId <> W.query findIDs file
  let orgContent = parseTopBlocks 0 $ W.walk removeIDs file
  noteContent <- mapM (A.block $ mapFamilyM $ mapOrgNote i orgIds title) orgContent
  updateImpl note $ pure . ((A.docContent .~ noteContent) . (A.docMtdt . at (mtdtName RoamRefs) ?~ toJSON orgIds))
  syncFileOfKind i (note ^. notePath) (entry ^. entryId) Kd.Note
  pure i

findIDs :: Block -> Set Text
findIDs (Header _ (i, _, _) _) | not (T.null i) = S.singleton i
findIDs _ = mempty

removeIDs :: Block -> Block
removeIDs (Header lvl (_, cls, attrs) content) = Header lvl ("", cls, attrs) content
removeIDs bk = bk

mapOrgNote :: (MonadKorrvigs m) => Id -> Set Text -> Text -> A.ASTPlate m
mapOrgNote note orgIds title =
  A.ASTPlate
    { A.block = pure,
      A.inline = importAttachments note orgIds title,
      A.header = pure . prepareHeader
    }

replaceAttachment :: (MonadKorrvigs m) => Id -> Set Text -> Text -> Text -> m (Maybe Id)
replaceAttachment note orgIds title attachment = flip firstJustM orgIds $ \orgId -> do
  let dir = "/home/luc/data/annex-bak/data/" <> T.take 2 orgId <> "/" <> T.drop 2 orgId
  let path = T.unpack $ dir <> "/" <> attachment
  let nentry = def & NE.neParents .~ [note] & NE.neTitle ?~ title
  ex <- liftIO $ doesFileExist path
  if ex
    then Just <$> NF.new path (def & NF.nfEntry .~ nentry)
    else pure Nothing

importAttachments :: (MonadKorrvigs m) => Id -> Set Text -> Text -> A.Inline -> m A.Inline
importAttachments note orgIds prefix (A.Link attr txt i vw) =
  case T.stripPrefix "attachment:" (unId i) of
    Nothing -> case T.stripPrefix "id:" (unId i) of
      Nothing -> pure $ A.Link attr txt i vw
      Just ref -> do
        other <- rSelectOne $ do
          entry <- selectTable entriesTable
          let sqlI = entry ^. sqlEntryId
          refs <- baseSelectMtdt RoamRefs sqlI
          rf <- sqlJsonElementsText $ toNullable refs
          where_ $ rf .== sqlStrictText ref
          pure $ entry ^. sqlEntryName
        case other of
          Nothing -> pure $ A.Styled A.Emph txt
          Just otherId -> pure $ A.Link attr txt otherId vw
    Just file ->
      replaceAttachment note orgIds title file >>= \case
        Nothing -> pure $ A.Styled A.Emph txt
        Just ai -> pure $ A.Link attr txt ai vw
  where
    title = prefix <> " - " <> renderInlines txt
importAttachments _ _ _ inl = pure inl

taskP :: (Stream s m Char) => ParsecT s u m TaskStatus
taskP =
  (ts "TASK" *> pure TaskTodo)
    <|> (ts "NEXT" *> pure TaskImportant)
    <|> (ts "WAITING" *> pure TaskBlocked)
    <|> (ts "STARTED" *> pure TaskOngoing)
    <|> (ts "DONE" *> pure TaskDone)
    <|> (ts "DONT" *> pure TaskDont)
  where
    ts = P.try . string

countP :: forall s u m. (Stream s m Char) => ParsecT s u m ()
countP =
  P.try $ do
    void $ char '['
    dec
    void $ char '/'
    dec
    void $ char ']'
    spaces
  where
    dec = void (decimal :: ParsecT s u m Int)

headerP :: (Stream s m Char) => ParsecT s u m (Maybe TaskStatus, Text)
headerP = do
  spaces
  tsk <- optionMaybe $ taskP <* spaces
  chars <- manyTill anyChar ending
  pure (tsk, T.pack chars)
  where
    ending = P.try $ spaces >> optional countP >> P.try (optional $ string "attach" >> spaces) >> eof

parseHdTitle :: Text -> (Maybe Task, Text)
parseHdTitle title = case runParser headerP () "hd title" title of
  Left _ -> (Nothing, title)
  Right (Nothing, ntitle) -> (Nothing, ntitle)
  Right (Just status, ntitle) ->
    let tsk =
          Task
            { _tskStatus = status,
              _tskStatusName = renderTaskStatus status,
              _tskLabel = ntitle,
              _tskDeadline = Nothing,
              _tskScheduled = Nothing,
              _tskStarted = Nothing,
              _tskFinished = Nothing
            }
     in (Just tsk, ntitle)

prepareHeader :: A.Header -> A.Header
prepareHeader hd =
  let (tsk, ntitle) = parseHdTitle (hd ^. A.hdTitle)
   in hd & A.hdTitle .~ ntitle & A.hdTask .~ tsk
