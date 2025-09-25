{-# LANGUAGE FlexibleContexts #-}

module Korrvigs.Link.New
  ( new,
    NewLink (..),
    nlEntry,
    nlOffline,
  )
where

import Conduit (throwM)
import Control.Applicative
import Control.Arrow (first)
import Control.Lens hiding (noneOf)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.CaseInsensitive as CI
import Data.Default
import qualified Data.Map as M
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New hiding (new)
import Korrvigs.Kind
import Korrvigs.Link.Download
import Korrvigs.Link.JSON
import Korrvigs.Link.SQL
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import Korrvigs.Metadata.Media.Ontology
import Korrvigs.Monad
import Korrvigs.Monad.Sync (syncFileOfKind)
import qualified Korrvigs.Note.New as Note
import Korrvigs.Utils (joinNull)
import Korrvigs.Utils.DateTree
import Korrvigs.Utils.JSON
import Network.URI
import Opaleye
import System.FilePath

data NewLink = NewLink
  { _nlEntry :: NewEntry,
    _nlOffline :: Bool
  }

makeLenses ''NewLink

instance Default NewLink where
  def = NewLink def False

new :: (MonadKorrvigs m) => Text -> NewLink -> m Id
new url options = do
  li <- rSelect $ do
    entry <- selectTable linksTable
    where_ $ entry ^. sqlLinkRef .== sqlStrictText url
    nameFor $ entry ^. sqlLinkId
  case li of
    (i : _) -> pure i
    [] -> create url options

create :: (MonadKorrvigs m) => Text -> NewLink -> m Id
create url options = case parseURI (T.unpack url) of
  Nothing -> throwM $ KMiscError $ "Could not parse URL: " <> url
  Just uri -> do
    let protocol = T.pack $ uriScheme uri
    let link = T.pack $ uriToString id uri ""
    extracted <-
      if options ^. nlOffline
        then pure mempty
        else downloadInformation link
    let nentry' = appEndo extracted $ options ^. nlEntry
    let title = joinNull T.null (nentry' ^. neTitle)
    let shouldBeNote = maybe False mediaTypeDefaultToNote $ nentry' ^? neMtdt . at (mtdtName MediaMtdt) . _Just . to fromJSONM . _Just
    case title of
      Just tit | shouldBeNote -> do
        let nnote = Note.NewNote nentry' tit False True
        Note.new nnote
      _ -> do
        nentry <- applyCover nentry' title
        dt <- useDate nentry Nothing
        let mtdt = useMtdt nentry M.empty
        let mtdtJson = M.fromList $ first CI.foldedCase <$> M.toList mtdt
        let txt = nentry ^. neContent
        let parents = unId <$> nentry ^. neParents
        let json = LinkJSON protocol link mtdtJson dt Nothing Nothing txt title parents
        idmk' <- applyNewEntry nentry (imk "link")
        let idmk =
              idmk'
                & idTitle .~ (title <|> Just (T.pack $ takeBaseName $ uriPath uri))
        i <- newId idmk
        rt <- linkJSONPath
        let jsonTT = linkJSONTreeType
        let content = encodePretty json
        pth <- storeFile rt jsonTT (nentry ^. neDate) (unId i <> ".json") $ FileLazy content
        syncFileOfKind pth Link
        applyOnNewEntry nentry i
        pure i
