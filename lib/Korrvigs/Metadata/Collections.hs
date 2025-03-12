module Korrvigs.Metadata.Collections
  ( ColTree (..),
    colEntries,
    colSubs,
    colTree,
    Favourite (..),
  )
where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Metadata
import Korrvigs.Metadata.TH
import Korrvigs.Monad
import Opaleye hiding (not, null)

data ColTree = ColTree
  { _colEntries :: [(Id, Maybe Text)],
    _colSubs :: Map Text ColTree
  }
  deriving (Eq, Show)

makeLenses ''ColTree

emptyTree :: ColTree
emptyTree = ColTree [] M.empty

insertIntoTree :: (Id, Maybe Text, [Text]) -> ColTree -> ColTree
insertIntoTree (i, title, []) fav = fav & colEntries %~ ((i, title) :)
insertIntoTree (i, title, cat : cats) fav =
  fav & colSubs . at cat %~ Just . insertIntoTree (i, title, cats) . fromMaybe emptyTree

buildTree :: [(Id, Maybe Text, [Text])] -> ColTree
buildTree = foldr insertIntoTree emptyTree

listCol ::
  (MonadKorrvigs m, ExtraMetadata mtdt, MtdtType mtdt ~ JSONList Text) =>
  mtdt ->
  [Text] ->
  Bool ->
  m [(Id, Maybe Text, [Text])]
listCol mtdt prefix recursive = do
  favs <- rSelect $ do
    fav <- selectTable entriesMetadataTable
    where_ $ fav ^. sqlKey .== sqlStrictText (mtdtSqlName mtdt)
    title <- selectTextMtdt Title $ fav ^. sqlEntry
    when (recursive && not (null prefix)) $ where_ $ matchPrefix $ fav ^. sqlValue
    unless recursive $ where_ $ fav ^. sqlValue .== sqlValueJSONB prefix
    pure (fav ^. sqlEntry, title, fav ^. sqlValue)
  pure $ mapMaybe prepJSON favs
  where
    prepJSON :: (Id, Maybe Text, Value) -> Maybe (Id, Maybe Text, [Text])
    prepJSON (i, title, val) = case fromJSON val of
      Success cats -> Just (i, title, unJSList cats)
      Error _ -> Just (i, title, [])
    matchPrefix :: Field SqlJsonb -> Field SqlBool
    matchPrefix js =
      foldr
        (\(i, txt) b -> b .&& matchNullable (sqlBool False) (.== sqlStrictText txt) (toNullable js .->> sqlInt4 i))
        (sqlBool True)
        $ zip [0 ..] prefix

colTree ::
  (MonadKorrvigs m, ExtraMetadata mtdt, MtdtType mtdt ~ JSONList Text) =>
  mtdt ->
  [Text] ->
  Bool ->
  m ColTree
colTree mtdt prefix recursive = buildTree <$> listCol mtdt prefix recursive

mkMtdt "Favourite" "favourite" [t|JSONList Text|]
