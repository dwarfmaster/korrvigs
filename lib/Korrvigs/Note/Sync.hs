module Korrvigs.Note.Sync where

import Control.Applicative
import Control.Arrow (first, second, (&&&))
import Control.Lens
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Lazy (writeFile)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.LocalTime
import Korrvigs.Compute.Runnable
import Korrvigs.Compute.SQL
import Korrvigs.Compute.Type
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Note.AST
import Korrvigs.Note.Code (toRunnable, updateRefInAttr)
import Korrvigs.Note.Helpers
import Korrvigs.Note.Pandoc
import Korrvigs.Note.Render (writeNoteLazy)
import Korrvigs.Note.SQL
import Korrvigs.Query
import Korrvigs.Utils (recursiveRemoveFile)
import Korrvigs.Utils.DateTree
import Korrvigs.Utils.JSON
import Korrvigs.Utils.Time
import Opaleye (Insert (..), doNothing, rCount, toFields)
import System.Directory (doesFileExist)
import System.FilePath (joinPath, takeBaseName)
import Prelude hiding (writeFile)

noteIdFromPath :: FilePath -> Id
noteIdFromPath = MkId . T.pack . takeBaseName

remove :: (MonadKorrvigs m) => Note -> m ()
remove note = do
  let path = note ^. notePath
  rt <- noteDirectory
  exists <- liftIO $ doesFileExist path
  when exists $ recursiveRemoveFile rt path

noteDirectory :: (MonadKorrvigs m) => m FilePath
noteDirectory = joinPath . (: ["notes"]) <$> root

noteTreeType :: DateTreeType
noteTreeType = def & dtYear .~ True & dtMonth .~ True

allNotes :: (MonadKorrvigs m) => m [FilePath]
allNotes = do
  rt <- noteDirectory
  let dtt = noteTreeType
  files <- listFiles rt dtt
  pure $ (^. _1) <$> files

list :: (MonadKorrvigs m) => m (Set FilePath)
list = S.fromList <$> allNotes

sync :: (MonadKorrvigs m) => m (Map Id SyncData)
sync =
  M.fromList <$> (allNotes >>= mapM (sequence . (noteIdFromPath &&& syncOne)))

syncOne :: (MonadKorrvigs m) => FilePath -> m SyncData
syncOne path = do
  let i = noteIdFromPath path
  doc <- readNote path >>= throwEither (KCantLoad i)
  syncDocument i path doc

syncDocument :: (MonadKorrvigs m) => Id -> FilePath -> Document -> m SyncData
syncDocument i path doc = do
  let mtdt = doc ^. docMtdt
  let geom = fromJSONM =<< mtdt ^. at "geometry"
  let day = dayToZonedTime utc <$> (fromJSONM =<< mtdt ^. at "date")
  let zt = fromJSONM =<< mtdt ^. at "date"
  let tm = zt <|> day
  let dur = fromJSONM =<< mtdt ^. at "duration"
  let title = Just $ doc ^. docTitle
  let erow = EntryRow Nothing Note i tm dur geom Nothing title :: EntryRowW
  let mtdt' = foldr M.delete mtdt ["geometry", "date", "duration"]
  let mrows = M.toList mtdt'
  let nrow sqlI = NoteRow sqlI path (M.keys $ doc ^. docCollections) :: NoteRow
  let cmps =
        doc
          ^.. docContent
            . each
            . bkSubBlocks
            . _CodeBlock
            . to (view (_1 . attrId) &&& uncurry toRunnable)
            . to raiseMaybe
            . _Just
            . filtered (not . T.null . view _1)
            . to (second runDeps)
  let insertNoteRow sqlI =
        Insert
          { iTable = notesTable,
            iRows = [toFields $ nrow sqlI],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  let crows sqlI = (\(col, items) -> NoteColRow sqlI col <$> items) =<< M.toList (doc ^. docCollections) :: [NoteColRow]
  let insertColRows sqlI =
        Insert
          { iTable = notesCollectionsTable,
            iRows = toFields <$> crows sqlI,
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  let txt = renderDocument doc
  pure $ SyncData erow (\sqlI -> [insertNoteRow sqlI, insertColRows sqlI]) mrows (Just txt) (S.toList $ doc ^. docParents) (S.toList $ doc ^. docRefTo) (M.fromList cmps)
  where
    raiseMaybe (_, Nothing) = Nothing
    raiseMaybe (a, Just b) = Just (a, b)

updateImpl' :: (MonadKorrvigs m) => Note -> (Document -> m (Document, a)) -> m a
updateImpl' note f = do
  let path = note ^. notePath
  let i = note ^. noteEntry . entryName
  doc <- readNote path >>= throwEither (KCantLoad i)
  (ndoc, r) <- f doc
  liftIO $ writeFile path $ writeNoteLazy ndoc
  pure r

updateImpl :: (MonadKorrvigs m) => Note -> (Document -> m Document) -> m ()
updateImpl note f = void $ updateImpl' note $ fmap (,()) . f

updateMetadata :: (MonadKorrvigs m) => Note -> Map Text Value -> [Text] -> m ()
updateMetadata note upd rm = updateImpl note $ pure . ndoc
  where
    updCi = M.fromList $ first CI.mk <$> M.toList upd
    rmCi = CI.mk <$> rm
    ndoc = docMtdt %~ M.union updCi . flip (foldr M.delete) rmCi

updateParents :: (MonadKorrvigs m) => Note -> [Id] -> [Id] -> m ()
updateParents note toAdd toRm = updateImpl note $ pure . upd
  where
    updParents = foldr (.) id $ fmap S.insert toAdd ++ fmap S.delete toRm
    upd = docParents %~ updParents

updateDate :: (MonadKorrvigs m) => Note -> Maybe ZonedTime -> m ()
updateDate note ntime =
  updateImpl note $ pure . (docMtdt . at "date" .~ (toJSON <$> ntime))

updateRef :: (MonadKorrvigs m) => Note -> Id -> Maybe Id -> m ()
updateRef note old new =
  updateImpl note $
    pure
      . (docMtdt %~ updateInMetadata old new)
      . (docParents %~ upd)
      . (docContent %~ mapMaybe (updateRefBlock old new))
  where
    upd = maybe id S.insert new . S.delete old

updateRefBlock :: Id -> Maybe Id -> Block -> Maybe Block
updateRefBlock old new (Para inls) = pure $ Para $ updateRefInline old new =<< inls
updateRefBlock old new (LineBlock inls) =
  pure $ LineBlock $ (>>= updateRefInline old new) <$> inls
updateRefBlock old new (CodeBlock attr code) =
  pure $ CodeBlock (updateRefInAttr old new attr) code
updateRefBlock old new (OrderedList cases) =
  pure $ OrderedList $ mapMaybe (updateRefBlock old new) <$> cases
updateRefBlock old new (BulletList cases) =
  pure $ BulletList $ mapMaybe (updateRefBlock old new) <$> cases
updateRefBlock old new (DefinitionList cases) =
  pure $ DefinitionList $ updateCase <$> cases
  where
    updateCase (inls, bks) = (updateRefInline old new =<< inls, mapMaybe (updateRefBlock old new) <$> bks)
updateRefBlock old new (Figure attr caption bks) =
  pure $ Figure attr (mapMaybe (updateRefBlock old new) caption) (mapMaybe (updateRefBlock old new) bks)
updateRefBlock old new (Embed i) | i == old = Embed <$> new
updateRefBlock old new (EmbedHeader i lvl) | i == old = EmbedHeader <$> new <*> pure lvl
updateRefBlock old new (Collection col i items) =
  pure $ Collection col i $ mapMaybe updateItem items
  where
    updateItem (ColItemEntry it) | it == old = ColItemEntry <$> new
    updateItem (ColItemInclude it txt) | it == old = ColItemInclude <$> new <*> pure txt
    updateItem (ColItemQuery q) = pure $ ColItemQuery $ updateQuery old new q
    updateItem (ColItemSubOf it) | it == old = ColItemSubOf <$> new
    updateItem c = pure c
updateRefBlock old new (Sub hd) =
  pure $ Sub $ hd & hdContent %~ mapMaybe (updateRefBlock old new)
updateRefBlock old new (Table tbl) =
  pure $ Table $ tbl & tableCells . each . cellData %~ mapMaybe (updateRefBlock old new)
updateRefBlock _ _ bk = pure bk

updateRefInline :: Id -> Maybe Id -> Inline -> [Inline]
updateRefInline old new (Styled style inls) =
  pure $ Styled style $ updateRefInline old new =<< inls
updateRefInline old new (Link attr inls i)
  | old == i =
      let ninls = updateRefInline old new =<< inls
       in case new of
            Nothing -> ninls
            Just nw -> [Link attr ninls nw]
updateRefInline old new (PlainLink (Just inls) uri) =
  pure $ PlainLink (Just $ updateRefInline old new =<< inls) uri
updateRefInline old new (Sidenote bks) =
  pure $ Sidenote $ mapMaybe (updateRefBlock old new) bks
updateRefInline _ _ inl = [inl]

updateQuery :: Id -> Maybe Id -> Query -> Query
updateQuery old new q =
  q
    & queryId %~ mapMaybe upd
    & querySubOf . _Just . relOther %~ updateQuery old new
    & queryParentOf . _Just . relOther %~ updateQuery old new
    & queryMentioning . _Just . relOther %~ updateQuery old new
    & queryMentionedBy . _Just . relOther %~ updateQuery old new
  where
    upd i | i == old = new
    upd i = Just i

updateTitle :: (MonadKorrvigs m) => Note -> Maybe Text -> m ()
updateTitle _ Nothing = pure ()
updateTitle note (Just ntitle) = updateImpl note $ pure . (docTitle .~ ntitle)

getComputation :: (MonadKorrvigs m) => Note -> Text -> m (Maybe Computation)
getComputation note cmp = do
  let i = note ^. noteEntry . entryName
  doc <- readNote (note ^. notePath) >>= throwEither (KCantLoad i)
  let code = doc ^? docContent . each . bkSubBlocks . _CodeBlock . filtered (\(attr, _) -> attr ^. attrId == cmp)
  let mrbl = code >>= uncurry toRunnable
  case mrbl of
    Nothing -> pure Nothing
    Just rbl ->
      pure $
        Just $
          Computation
            { _cmpEntry = i,
              _cmpName = cmp,
              _cmpRun = rbl,
              _cmpResult = doc ^. docComputations . at cmp
            }

storeComputationResult :: (MonadKorrvigs m) => Note -> Text -> RunnableType -> Hash -> RunnableResult -> m ()
storeComputationResult note cmp tp hash res =
  updateImpl note $ pure . (docComputations . at cmp ?~ (tp, hash, res))
