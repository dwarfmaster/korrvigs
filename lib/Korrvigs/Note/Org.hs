module Korrvigs.Note.Org (importOrgFile) where

import Control.Exception
import Control.Lens
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
import Korrvigs.Monad
import Korrvigs.Monad.Sync (syncFileOfKind)
import qualified Korrvigs.Note.AST as A
import Korrvigs.Note.Helpers (renderInlines)
import qualified Korrvigs.Note.New as NN
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Sync (updateImpl)
import Korrvigs.Utils.Pandoc
import System.Directory
import Text.Pandoc
import qualified Text.Pandoc.Walk as W

importOrgFile :: (MonadKorrvigs m) => NE.NewEntry -> FilePath -> m Id
importOrgFile ne org = $withLogContext ("Importing org file \"" <> T.pack org <> "\"") $ do
  contentE <- liftIO $ (try $ TIO.readFile org :: IO (Either SomeException Text))
  content <- throwEither (\e -> KMiscError . T.pack $ "IO error: " <> show e) contentE
  fileE <- liftIO $ runIO $ readOrg def content
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
  noteContent <- forMOf (each . A.bkSubBlocks . A.bkInlines) orgContent (importAttachments i orgIds title)
  updateImpl note $ pure . (A.docContent .~ noteContent)
  syncFileOfKind i (note ^. notePath) (entry ^. entryId) Kd.Note
  pure i

findIDs :: Block -> Set Text
findIDs (Header _ (i, _, _) _) | not (T.null i) = S.singleton i
findIDs _ = mempty

removeIDs :: Block -> Block
removeIDs (Header lvl (_, cls, attrs) content) = Header lvl ("", cls, attrs) content
removeIDs bk = bk

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
    Nothing -> pure $ A.Link attr txt i vw
    Just file ->
      replaceAttachment note orgIds title file >>= \case
        Nothing -> pure $ A.Styled A.Emph txt
        Just ai -> pure $ A.Link attr txt ai vw
  where
    title = prefix <> " - " <> renderInlines txt
importAttachments _ _ _ inl = pure inl
