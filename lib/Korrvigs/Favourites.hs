module Korrvigs.Favourites (FavouriteTree (..), favEntries, favSubs, favTree) where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.JSON (sqlJsonToText)
import Opaleye

data FavouriteTree = FavTree
  { _favEntries :: [(Id, Maybe Text)],
    _favSubs :: Map Text FavouriteTree
  }
  deriving (Eq, Show)

makeLenses ''FavouriteTree

emptyTree :: FavouriteTree
emptyTree = FavTree [] M.empty

insertIntoTree :: (Id, Maybe Text, [Text]) -> FavouriteTree -> FavouriteTree
insertIntoTree (i, title, []) fav = fav & favEntries %~ ((i, title) :)
insertIntoTree (i, title, cat : cats) fav =
  fav & favSubs . at cat %~ Just . insertIntoTree (i, title, cats) . fromMaybe emptyTree

buildTree :: [(Id, Maybe Text, [Text])] -> FavouriteTree
buildTree = foldr insertIntoTree emptyTree

listFavs :: (MonadKorrvigs m) => m [(Id, Maybe Text, [Text])]
listFavs = do
  favs <- rSelect $ do
    fav <- selectTable entriesMetadataTable
    where_ $ fav ^. sqlKey .== sqlStrictText "favourite"
    title <- optional $ limit 1 $ do
      mtdt <- selectTable entriesMetadataTable
      where_ $ (mtdt ^. sqlEntry) .== (fav ^. sqlEntry)
      where_ $ mtdt ^. sqlKey .== sqlStrictText "title"
      let titleText = sqlJsonToText $ toNullable $ mtdt ^. sqlValue
      pure titleText
    pure (fav ^. sqlEntry, title, fav ^. sqlValue)
  pure $ mapMaybe prepJSON $ favs & each . _2 %~ join
  where
    prepJSON :: (Id, Maybe Text, Value) -> Maybe (Id, Maybe Text, [Text])
    prepJSON (i, title, val) = case fromJSON val of
      Success cats -> Just (i, title, cats)
      Error _ -> Just (i, title, [])

favTree :: (MonadKorrvigs m) => m FavouriteTree
favTree = buildTree <$> listFavs
