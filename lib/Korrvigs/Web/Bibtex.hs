module Korrvigs.Web.Bibtex
  ( getSearchBibtexR,
    getEntryBibtexR,
    getNoteColBibtexR,
  )
where

import Control.Lens
import qualified Data.Binary.Builder as Bld
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime
import Korrvigs.Entry
import qualified Korrvigs.Metadata.Media.Export as E
import Korrvigs.Monad
import Korrvigs.Monad.Collections
import Korrvigs.Note hiding (subs)
import Korrvigs.Query
import Korrvigs.Web.Backend
import Korrvigs.Web.Routes
import Korrvigs.Web.Search.Form
import Opaleye
import Yesod

getSearchBibtexR :: Handler TypedContent
getSearchBibtexR = do
  tz <- liftIO getCurrentTimeZone
  let mktz = fmap $ flip ZonedTime tz
  query <- runInputGet $ queryForm mktz Nothing
  ids <- rSelect $ do
    entry <- compile query
    pure $ entry ^. sqlEntryName
  getBibtex "query" ids

getEntryBibtexR :: WebId -> Handler TypedContent
getEntryBibtexR (WId i) = do
  subs <- rSelect $ do
    src <- fromName (selectSourcesFor entriesSubTable) $ sqlId i
    entry <- selectTable entriesTable
    where_ $ entry ^. sqlEntryId .== src
    pure $ entry ^. sqlEntryName
  getBibtex (unId i) subs

getNoteColBibtexR :: WebId -> Text -> Handler TypedContent
getNoteColBibtexR (WId i) col = do
  ids <- loadIDs $ ColItemInclude i col
  getBibtex (unId i <> "_" <> col) ids
  where
    loadIDs :: CollectionItem -> Handler [Id]
    loadIDs (ColItemEntry ci) = pure [ci]
    loadIDs (ColItemInclude nt ncol) = do
      items <- noteCollection nt ncol
      ids <- mapM loadIDs $ fromMaybe [] items
      pure $ mconcat ids
    loadIDs (ColItemQuery q) = rSelect $ do
      entry <- compile q
      pure $ entry ^. sqlEntryName
    loadIDs (ColItemSubOf ni) = rSelect $ do
      src <- fromName (selectSourcesFor entriesSubTable) $ sqlId ni
      entry <- selectTable entriesTable
      where_ $ entry ^. sqlEntryId .== src
      pure $ entry ^. sqlEntryName
    loadIDs (ColItemComment _) = pure []

getBibtex :: Text -> [Id] -> Handler TypedContent
getBibtex nm ids = do
  bib <- E.export ids
  let content = ContentBuilder (Bld.fromLazyByteString bib) $ Just $ fromIntegral $ BSL.length bib
  addHeader "Content-Disposition" $ "attachment; filename=\"" <> nm <> ".bib\""
  pure $ toTypedContent ("text/x-bibtex" :: BS.ByteString, content)
