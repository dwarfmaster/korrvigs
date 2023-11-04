module Korrvigs.Web.Entry.OntologyClass (treeWidget, treeWidgetForEntry) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Schema
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import Opaleye ((.==))
import qualified Opaleye as O
import Yesod (liftIO, notFound, whamlet)

classesEntry :: Handler (Array Class Entry)
classesEntry = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn sql
  pure $ A.array (minBound, maxBound) $ map prepare res
  where
    sql :: O.Select (O.Field O.SqlText, O.Field O.SqlUuid, O.Field O.SqlText, O.Field O.SqlText)
    sql = do
      (class_, entry_) <- O.selectTable classEntryTable
      (id_, name_, notes_) <- O.selectTable entriesTable
      O.where_ $ entry_ .== id_
      pure (class_, id_, name_, notes_)
    prepare :: (Text, UUID, Text, Text) -> (Class, Entry)
    prepare (className, uuid, nm, notes) =
      (fromJust $ parse className, MkEntry uuid nm $ T.unpack notes)

treeWidget :: Maybe Class -> Handler Widget
treeWidget mcls = do
  entries <- classesEntry
  pure $ Rcs.classTree >> [whamlet|<ul #classes-tree> ^{makeTree entries Entity}|]
  where
    allClasses :: [Class]
    allClasses = sortBy (\a b -> compare (name a) (name b)) [minBound .. maxBound]
    unfolded :: Set Class
    unfolded = Set.fromList $
      case mcls of
        Just Entity -> []
        Just cls ->
          let f c = if c == Entity then [Entity] else (c : f (isA c))
           in f (isA cls)
        Nothing -> allClasses
    childrenOf :: Class -> [Class]
    childrenOf cls = filter (\c -> isA c == cls && c /= Entity) allClasses
    makeTree :: Array Class Entry -> Class -> Widget
    makeTree entries cls =
      Rcs.classTreeSub
        (name cls)
        (entry_id $ entries ! cls)
        (not $ Set.member cls unfolded)
        $ map (makeTree entries)
        $ childrenOf cls

treeWidgetForEntry :: Entry -> Handler Widget
treeWidgetForEntry entry = do
  conn <- pgsql
  res <- liftIO $ map parse <$> O.runSelect conn sql
  case res of
    [Just cls] -> treeWidget (Just cls)
    _ -> notFound
  where
    sql :: O.Select (O.Field O.SqlText)
    sql = do
      (class_, entry_) <- O.selectTable classEntryTable
      O.where_ $ entry_ .== O.sqlUUID (entry_id entry)
      return class_
