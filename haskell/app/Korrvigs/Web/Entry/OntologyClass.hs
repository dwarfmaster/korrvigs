module Korrvigs.Web.Entry.OntologyClass (treeWidget, widgetsForClassEntry) where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (writeFile)
import Data.UUID (UUID)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Schema
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.UUID as U
import Network.HTTP.Types (Method, methodGet, methodPost)
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Yesod hiding (Entity)
import Prelude hiding (writeFile)

--  _____

-- | _   _| __ ___  ___
--    | || '__/ _ \/ _ \
--    | || | |  __/  __/
--    |_||_|  \___|\___|
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

--  ___           _

-- | _ _|_ __  ___| |_ __ _ _ __   ___ ___  ___
--   | || '_ \/ __| __/ _` | '_ \ / __/ _ \/ __|
--   | || | | \__ \ || (_| | | | | (_|  __/\__ \
--  |___|_| |_|___/\__\__,_|_| |_|\___\___||___/
classInstances :: Class -> Handler [Entry]
classInstances cls = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn $ do
    (id_, name_, notes_) <- O.selectTable entriesTable
    (_, class_, uuid_, sub_, query_) <- O.selectTable entitiesTable
    O.where_ $ O.isNull sub_ .&& O.isNull query_
    O.where_ $ uuid_ .== id_
    O.where_ $ class_ .== O.sqlStrictText (name cls)
    pure (id_, name_, notes_)
  pure $ (\(i, nm, md) -> MkEntry i nm $ T.unpack md) <$> res

classInstancesWidget :: Class -> Handler (Maybe Widget)
classInstancesWidget cls = do
  instances <- classInstances cls
  pure $ if null instances then Nothing else Just $ Rcs.classInstances instances

--  _   _                 ___           _

-- | \ | | _____      __ |_ _|_ __  ___| |_ __ _ _ __   ___ ___
-- |  \| |/ _ \ \ /\ / /  | || '_ \/ __| __/ _` | '_ \ / __/ _ \
-- | |\  |  __/\ V  V /   | || | | \__ \ || (_| | | | | (_|  __/
-- |_| \_|\___| \_/\_/   |___|_| |_|___/\__\__,_|_| |_|\___\___|
data NewInstance = NewInstance Class Text Text

newInstanceForm :: Class -> Html -> MForm Handler (FormResult NewInstance, Widget)
newInstanceForm cls =
  identifyForm "NewInstance" $
    renderDivs $
      NewInstance cls
        <$> areq textField "Name" Nothing
        <*> (unTextarea <$> areq textareaField "Description" Nothing)

newInstanceWidget :: UUID -> Class -> Handler Widget
newInstanceWidget uuid cls = do
  (widget, enctype) <- generateFormPost $ newInstanceForm cls
  pure $
    Rcs.formStyle
      >> [whamlet|
           <form #class-new-instance method="post" action=@{EntryR (U.UUID uuid)} enctype=#{enctype}>
             ^{widget}
             <button type="submit">Create
         |]

newInstance :: NewInstance -> Handler a
newInstance (NewInstance cls nm desc) = do
  conn <- pgsql
  root <- korrRoot
  let mdName = mkMdName nm
  entry <- newEntry conn root cls nm mdName
  let mdPath = entryMdPath root entry
  liftIO $ writeFile mdPath desc
  redirect $ EntryR $ U.UUID $ entry_id entry

newInstanceProcess :: Class -> Handler Widget
newInstanceProcess cls = do
  ((result, _), _) <- runFormPost $ newInstanceForm cls
  case result of
    FormSuccess form -> newInstance form
    FormFailure err ->
      pure $
        [whamlet|
        <p>Invalid input:
          <ul>
            $forall msg <- err
              <li> #{msg}
      |]
    FormMissing -> pure $ pure ()

--  ____  _                 _     _

-- |  _ \| |_   _ _ __ ___ | |__ (_)_ __   __ _
-- | |_) | | | | | '_ ` _ \| '_ \| | '_ \ / _` |
-- |  __/| | |_| | | | | | | |_) | | | | | (_| |
-- |_|   |_|\__,_|_| |_| |_|_.__/|_|_| |_|\__, |
--                                        |___/
sqlFindClass :: UUID -> O.Select (O.Field O.SqlText)
sqlFindClass uuid = do
  (class_, entry_) <- O.selectTable classEntryTable
  O.where_ $ entry_ .== O.sqlUUID uuid
  return class_

widgetsForClassEntry :: Method -> Entry -> Handler (Map String Widget)
widgetsForClassEntry method entry = do
  conn <- pgsql
  res <- liftIO $ map parse <$> O.runSelect conn (sqlFindClass $ entry_id entry)
  case res of
    [Just cls] -> do
      tree <- select methodGet "Class tree" $ treeWidget (Just cls)
      instances <- selectOpt methodGet "Instances" $ classInstancesWidget cls
      newInst <-
        select methodGet "New instance" $
          newInstanceWidget (entry_id entry) cls
      newInst' <-
        select methodPost "New instance" $
          newInstanceProcess cls
      pure $ M.fromList $ tree ++ instances ++ newInst ++ newInst'
    _ -> notFound
  where
    select :: Method -> String -> Handler Widget -> Handler [(String, Widget)]
    select met nm widgetM
      | method == met =
          widgetM >>= \widget -> pure $ [(nm, widget)]
    select _ _ _ = pure []
    selectOpt :: Method -> String -> Handler (Maybe Widget) -> Handler [(String, Widget)]
    selectOpt met nm widgetM
      | method == met =
          widgetM >>= \widgetO -> pure $ case widgetO of
            Just widget -> [(nm, widget)]
            Nothing -> []
    selectOpt _ _ _ = pure []
