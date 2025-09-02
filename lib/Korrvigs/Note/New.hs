module Korrvigs.Note.New (new, NewNote (..), nnEntry, nnTitle, nnTitleOverride, nnIgnoreUrl) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Foldable
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New hiding (new)
import Korrvigs.Kind
import qualified Korrvigs.Link.Download as Link
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Monad
import Korrvigs.Monad.Sync
import Korrvigs.Note.AST
import Korrvigs.Note.Render
import Korrvigs.Note.Sync
import Korrvigs.Utils.DateTree
import Network.URI (parseURI)
import Opaleye hiding (not, null)

data NewNote = NewNote
  { _nnEntry :: NewEntry,
    _nnTitle :: Text,
    _nnTitleOverride :: Bool,
    _nnIgnoreUrl :: Bool
  }

makeLenses ''NewNote

new :: (MonadKorrvigs m) => NewNote -> m Id
new note = do
  mi <- rSelect $ do
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryKind .== sqlKind Note
    t <- baseSelectTextMtdt Title $ entry ^. sqlEntryName
    where_ $ t .== sqlStrictText (note ^. nnTitle)
    pure $ entry ^. sqlEntryName
  case mi of
    (i : _) -> pure i
    [] -> create note

initContent :: Map (CI Text) Value -> [Block]
initContent mtdt =
  mconcat
    [ [Para [PlainLink Nothing uri] | url <- get Url, uri <- toList (parseURI $ T.unpack url)],
      [Embed (MkId i) | i <- get Cover],
      [Para (mkContent txt) | txt <- get Abstract]
    ]
  where
    get :: (ExtraMetadata mt, MtdtType mt ~ a, FromJSON a) => mt -> [a]
    get m = toList $ extractMtdt m mtdt
    mkContent :: Text -> [Inline]
    mkContent = intersperse Space . fmap Plain . filter (not . T.null) . T.split (== ' ')

create :: (MonadKorrvigs m) => NewNote -> m Id
create note = do
  extracted <- case note ^? nnEntry . neMtdt . at (mtdtName Url) . _Just . _String of
    Just url | not (note ^. nnIgnoreUrl) -> Link.downloadInformation url
    _ -> pure mempty
  let nentry' = appEndo extracted $ note ^. nnEntry
  let title =
        if note ^. nnTitleOverride
          then fromMaybe (note ^. nnTitle) $ nentry' ^? neMtdt . at (mtdtName Title) . _Just . _JSON
          else note ^. nnTitle
  nentry <- applyCover nentry' $ Just title
  idmk' <- applyNewEntry nentry (imk "note")
  let idmk = idmk' & idTitle ?~ title
  i <- newId idmk
  let parents = nentry ^. neParents
  let mtdt =
        useMtdt nentry $
          mconcat
            [ M.singleton (mtdtName Title) (toJSON title),
              maybe M.empty (M.singleton (mtdtName Language) . toJSON) (nentry ^. neLanguage),
              if null parents then M.empty else M.singleton (CI.mk "parents") (toJSON $ unId <$> parents),
              maybe M.empty (M.singleton (CI.mk "date") . toJSON) (nentry ^. neDate)
            ]
  let doc =
        Document
          { _docMtdt = mtdt,
            _docContent = initContent mtdt,
            _docTitle = title,
            _docRefTo = S.empty,
            _docChecks = def,
            _docParents = S.fromList $ nentry ^. neParents,
            _docTask = Nothing,
            _docTasks = [],
            _docCollections = S.empty,
            _docNamedSubs = S.empty,
            _docNamedCode = S.empty
          }
  let bs = writeNoteLazy doc
  rt <- noteDirectory
  path <- storeFile rt noteTreeType (nentry ^. neDate) (unId i <> ".md") $ FileLazy bs
  syncFileOfKind path Note
  applyCollections nentry i
  applyChildren nentry i
  pure i
