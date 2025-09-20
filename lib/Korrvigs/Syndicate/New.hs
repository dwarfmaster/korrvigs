module Korrvigs.Syndicate.New where

import Control.Arrow (first)
import Control.Lens hiding (noneOf)
import Data.Aeson (toJSON)
import Data.Aeson.Encoding (encodingToLazyByteString, value)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Entry.New
import Korrvigs.File.New
import Korrvigs.Kind
import Korrvigs.Monad
import Korrvigs.Monad.Sync (syncFileOfKind)
import Korrvigs.Syndicate.JSON
import Korrvigs.Syndicate.SQL
import Korrvigs.Syndicate.Sync
import Korrvigs.Utils (joinNull)
import Korrvigs.Utils.DateTree (FileContent (..), storeFile)
import Opaleye

data NewSyndicate = NewSyndicate
  { _nsEntry :: NewEntry,
    _nsUrl :: Text,
    _nsFilter :: Maybe (Id, Text)
  }

makeLenses ''NewSyndicate

new :: (MonadKorrvigs m) => NewSyndicate -> m Id
new ns = do
  si <- rSelectOne $ do
    entry <- selectTable syndicatesTable
    where_ $ entry ^. sqlSynUrl .== sqlStrictText (ns ^. nsUrl)
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
  let content = encodingToLazyByteString . value $ toJSON json
  pth <- storeFile rt jsonTT (nentry ^. neDate) (unId i <> ".json") $ FileLazy content
  syncFileOfKind pth Syndicate
  applyCollections nentry i
  applyChildren nentry i
  pure i
