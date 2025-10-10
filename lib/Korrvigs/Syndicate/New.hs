module Korrvigs.Syndicate.New where

import Control.Applicative
import Control.Arrow (first)
import Control.Lens hiding (noneOf)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.CaseInsensitive as CI
import Data.Default
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Kind
import Korrvigs.Link.SQL
import Korrvigs.Metadata
import Korrvigs.Metadata.Media
import qualified Korrvigs.Metadata.Media.New as Media
import Korrvigs.Monad
import qualified Korrvigs.Monad.Metadata as Mtdt
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.SQL
import Korrvigs.Syndicate.Sync
import Korrvigs.Utils (joinNull)
import Korrvigs.Utils.DateTree (FileContent (..), storeFile)
import Opaleye

data NewSyndicate = NewSyndicate
  { _nsEntry :: NewEntry,
    _nsUrl :: Maybe Text,
    _nsFilter :: Maybe (Id, Text)
  }

makeLenses ''NewSyndicate

new :: (MonadKorrvigs m) => NewSyndicate -> m Id
new ns = do
  si <- rSelectOne $ do
    entry <- selectTable syndicatesTable
    forM_ (ns ^. nsUrl) $ \url ->
      where_ $
        matchNullable (sqlBool False) (.== sqlStrictText url) $
          entry ^. sqlSynUrl
    nameFor $ entry ^. sqlSynId
  case si of
    Nothing -> create ns
    Just i -> pure i

create :: (MonadKorrvigs m) => NewSyndicate -> m Id
create ns = do
  nentry <- applyCover (ns ^. nsEntry) Nothing
  dt <- useDate nentry Nothing
  let mtdt = useMtdt nentry M.empty
  let mtdtJson = M.fromList $ first CI.foldedCase <$> M.toList mtdt
  let txt = nentry ^. neContent
  let parents = unId <$> nentry ^. neParents
  let title = joinNull T.null $ nentry ^. neTitle
  let json =
        SyndicateJSON
          { _synjsUrl = ns ^. nsUrl,
            _synjsETag = Nothing,
            _synjsFilter = ns ^. nsFilter,
            _synjsExpiration = Nothing,
            _synjsItems = [],
            _synjsMetadata = mtdtJson,
            _synjsDate = dt,
            _synjsDuration = Nothing,
            _synjsGeo = Nothing,
            _synjsText = txt,
            _synjsTitle = title,
            _synjsParents = parents
          }
  idmk <- applyNewEntry nentry $ imk "syn"
  i <- newId idmk
  rt <- synJSONPath
  let jsonTT = synTreeType
  let content = encodePretty json
  pth <- storeFile rt jsonTT (nentry ^. neDate) (unId i <> ".json") $ FileLazy content
  syncFileOfKind pth Syndicate
  applyOnNewEntry nentry i
  pure i

lookupFromUrl :: (MonadKorrvigs m) => Text -> m (Maybe Id)
lookupFromUrl url = do
  lnk <- rSelectOne $ do
    lnk <- selectTable linksTable
    where_ $ lnk ^. sqlLinkRef .== sqlStrictText url
    nameFor $ lnk ^. sqlLinkId
  entry <- rSelectOne $ do
    entry <- selectTable entriesTable
    u <- baseSelectTextMtdt Url $ entry ^. sqlEntryId
    where_ $ u .== sqlStrictText url
    pure $ entry ^. sqlEntryName
  pure $ entry <|> lnk

lazyUpdateDate :: (MonadKorrvigs m) => Id -> Maybe UTCTime -> m ()
lazyUpdateDate i time = do
  entry <- load i >>= throwMaybe (KCantLoad i "Failed to load entry when importing item")
  when (isNothing $ entry ^. entryDate) $
    forM_ time $ \dt -> do
      tz <- liftIO getCurrentTimeZone
      Mtdt.updateDate entry $ Just $ utcToZonedTime tz dt

newFromItem :: (MonadKorrvigs m) => Syndicate -> Int -> m Id
newFromItem syn itemSeq = do
  let sqlI = syn ^. synEntry . entryId
  itemM <- rSelectOne $ do
    item <- selectTable syndicatedItemsTable
    where_ $ item ^. sqlSynItSyndicate .== sqlInt4 sqlI
    where_ $ item ^. sqlSynItSequence .== sqlInt4 itemSeq
    pure item
  item :: SyndicateItemRow <- flip throwMaybe itemM $ KMiscError $ "Syndicate " <> unId (syn ^. synEntry . entryName) <> " has no item #" <> T.pack (show itemSeq)
  case item ^. sqlSynItInstance of
    Just i -> pure $ MkId i
    Nothing -> do
      oldI <- lookupFromUrl $ item ^. sqlSynItUrl
      i <- case oldI of
        Just i -> do
          lazyUpdateDate i $ item ^. sqlSynItDate
          pure i
        Nothing -> do
          let nmedia =
                Media.NewMedia
                  { Media._nmEntry =
                      def
                        & neTitle ?~ (item ^. sqlSynItTitle)
                        & neDate %~ maybe id (const . Just . utctDay) (item ^. sqlSynItDate),
                    Media._nmInput = item ^. sqlSynItUrl,
                    Media._nmType = Nothing,
                    Media._nmCapture = True
                  }
          Media.new nmedia
      instantiateItem syn itemSeq i
      pure i
