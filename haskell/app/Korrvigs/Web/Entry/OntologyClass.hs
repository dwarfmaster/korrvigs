{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Web.Entry.OntologyClass (treeWidget, widgetsForClassEntry, newClass) where

import Control.Monad (void)
import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Int (Int64)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import qualified Korrvigs.DB as DB
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Pandoc (renderUrl)
import Korrvigs.Schema
import Korrvigs.Tree (writeNotes)
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import Korrvigs.Web.Entry.Types
import Korrvigs.Web.Form
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.UUID as U
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Text.Pandoc.Builder (Blocks, bulletList, link, para, rawBlock, text)
import Yesod hiding (Entity)

--  _____

-- | _   _| __ ___  ___
--    | || '__/ _ \/ _ \
--    | || | |  __/  __/
--    |_||_|  \___|\___|
classesEntry :: Handler (Array Class Entry)
classesEntry = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn sql
  pure $ A.array (minBound, maxBound) $ mapMaybe prepare res
  where
    sql :: O.Select (O.Field O.SqlText, O.Field O.SqlUuid, O.Field O.SqlText, O.Field O.SqlText, O.Field O.SqlInt8)
    sql = do
      (class_, entry_) <- O.selectTable classEntryTable
      (id_, name_, notes_) <- O.selectTable entriesTable
      (eid_, _) <- DB.rootFor id_
      O.where_ $ entry_ .== id_
      pure (class_, id_, name_, notes_, eid_)
    prepare :: (Text, UUID, Text, Text, Int64) -> Maybe (Class, Entry)
    prepare (className, uuid, nm, notes, eid) =
      (,MkEntry uuid nm (T.unpack notes) $ MkEntity eid OntologyClass uuid Nothing Nothing)
        <$> parse className

treeWidget :: Maybe Class -> Handler WidgetMap
treeWidget mcls = do
  entries <- classesEntry
  let widget = Rcs.classTree >> [whamlet|<ul #classes-tree> ^{makeTree entries Entity}|]
  pure $
    M.fromList
      [ ("Class tree", Right (widget, trivialHandler))
      ]
  where
    allClasses :: [Class]
    allClasses = sortBy (\a b -> compare (name a) (name b)) [minBound .. maxBound]
    unfolded :: Set Class
    unfolded = Set.fromList $
      case mcls of
        Just Entity -> []
        Just cls ->
          let f c = if c == Entity then [Entity] else c : f (isA c)
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
    (eid_, class_) <- DB.rootFor id_
    O.where_ $ class_ .== O.sqlStrictText (name cls)
    pure (id_, name_, notes_, eid_)
  pure $ (\(i, nm, md, eid) -> MkEntry i nm (T.unpack md) $ MkEntity eid cls i Nothing Nothing) <$> res

classInstancesWidget :: Class -> Handler WidgetMap
classInstancesWidget cls = do
  instances <- classInstances cls
  pure $
    if null instances
      then M.empty
      else M.singleton "Instances" $ Left $ bulletList $ mkLink <$> instances
  where
    mkLink :: Entry -> Blocks
    mkLink entry = para $ link (renderUrl $ entryRef entry) (entry_name entry) $ text $ entry_name entry

--  _   _                  ____ _

-- | \ | | _____      __  / ___| | __ _ ___ ___
-- |  \| |/ _ \ \ /\ / / | |   | |/ _` / __/ __|
-- | |\  |  __/\ V  V /  | |___| | (_| \__ \__ \
-- |_| \_|\___| \_/\_/    \____|_|\__,_|___/___/
data NewClass = NewClass Text Text Text

newClassForm :: Text -> Html -> MForm Handler (FormResult NewClass, Widget)
newClassForm cls =
  nameDescForm "Class" $ NewClass cls

newClassWidget :: UUID -> Text -> Handler Widget
newClassWidget uuid cls = do
  (widget, enctype) <- generateFormPost $ newClassForm cls
  pure $
    Rcs.formStyle
      >> [whamlet|
           <form #class-new-sub method="post" action=@{EntryR (U.UUID uuid)} enctype=#{enctype}>
             <input type="hidden" name="widget" value="New Subclass Widget">
             ^{widget}
             <button type="submit">Create
         |]

doNewClass :: NewClass -> Handler a
doNewClass (NewClass cls nm desc) = newClass cls nm desc

newClass :: Text -> Text -> Text -> Handler a
newClass cls nm desc = do
  conn <- pgsql
  root <- korrRoot
  let mdName = mkMdName nm
  entry <- liftIO $ withTransaction conn $ do
    entry <- newEntry conn root OntologyClass nm mdName
    void $
      O.runInsert conn $
        O.Insert
          { O.iTable = classesTable,
            O.iRows = [(O.sqlStrictText nm, O.sqlStrictText cls)],
            O.iReturning = O.rCount,
            O.iOnConflict = Nothing
          }
    void $
      O.runInsert conn $
        O.Insert
          { O.iTable = classEntryTable,
            O.iRows = [(O.sqlStrictText nm, O.sqlUUID $ entry_id entry)],
            O.iReturning = O.rCount,
            O.iOnConflict = Nothing
          }
    pure entry
  writeNotes root entry desc
  redirect $ EntryR $ U.UUID $ entry_id entry

newClassProcess :: Text -> Handler TypedContent
newClassProcess cls = do
  ((result, _), _) <- runFormPost $ newClassForm cls
  case result of
    FormSuccess form -> doNewClass form
    FormFailure err ->
      pure $ toTypedContent $ toJSON err
    FormMissing -> pure $ toTypedContent ()

newClassMap :: UUID -> Text -> Handler WidgetMap
newClassMap entry cls = do
  form <- newClassWidget entry cls
  pure $
    M.fromList
      [ ("New Subclass", Left $ rawBlock "widget" "New Subclass Widget"),
        ("New Subclass Widget", Right (form, newClassProcess cls))
      ]

--  ____  _                 _     _

-- |  _ \| |_   _ _ __ ___ | |__ (_)_ __   __ _
-- | |_) | | | | | '_ ` _ \| '_ \| | '_ \ / _` |
-- |  __/| | |_| | | | | | | |_) | | | | | (_| |
-- |_|   |_|\__,_|_| |_| |_|_.__/|_|_| |_|\__, |
--                                        |___/
widgetsForClassEntry :: Entry -> Handler WidgetMap
widgetsForClassEntry entry = do
  res <- findClass $ entry_id entry
  case res of
    Just cls -> do
      tree <- treeWidget (Just cls)
      instances <- classInstancesWidget cls
      newCls <- newClassMap (entry_id entry) $ name cls
      pure $ mconcat [tree, instances, newCls]
    Nothing -> do
      conn <- pgsql
      parent :: [(Text, UUID)] <- liftIO $ O.runSelect conn $ do
        (cls_, parent_) <- O.selectTable classesTable
        O.where_ $ cls_ .== O.sqlStrictText (entry_name entry)
        (id_, name_, _) <- O.selectTable entriesTable
        O.where_ $ name_ .== parent_
        (_, class_, uuid_, sub_, query_) <- O.selectTable entitiesTable
        O.where_ $ O.isNull sub_ .&& O.isNull query_ .&& (uuid_ .== id_)
        O.where_ $ class_ .== O.sqlStrictText (name OntologyClass)
        pure (parent_, id_)
      case parent of
        [(parentName, parentId)] -> do
          let parentW =
                [whamlet|
                <a href=@{EntryR (U.UUID parentId)}>
                  #{parentName}
            |]
          let parents = [("Parent", parentW)]
          let generate =
                [whamlet|
              <a href=@{GenerateClassesR}>
                Regenerate haskell classes
            |]
          let generates = [("Generate", generate)]
          -- newCls <-
          --   select (method == methodGet) "New subclass" $
          --     newClassWidget (entry_id entry) $
          --       entry_name entry
          -- newCls' <-
          --   select (method == methodPost) "New subclass" $
          --     newClassProcess $
          --       entry_name entry
          -- pure $ M.fromList $ parents ++ newCls ++ newCls' ++ generates
          pure M.empty
        _ -> notFound
