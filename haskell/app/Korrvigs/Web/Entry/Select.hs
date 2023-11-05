module Korrvigs.Web.Entry.Select (getAllEntriesR) where

import qualified Data.Aeson as JSON
import Data.Text (Text)
import qualified Data.UUID as U
import Korrvigs.Classes
import Korrvigs.Classes.Colors
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Korrvigs.Web.Header
import qualified Korrvigs.Web.Ressources as Rcs
import qualified Korrvigs.Web.UUID as UUID
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Yesod

entryToJson :: U.UUID -> Text -> Text -> Handler JSON.Value
entryToJson i nm clsName = do
  render <- getUrlRender
  pure $
    JSON.object
      [ "url" .= render (EntryR $ UUID.UUID i),
        "class" .= clsName,
        "color" .= ("var(--base" ++ clsColor ++ ")"),
        "name" .= nm
      ]
  where
    clsColor = case parse clsName of
      Just cls -> classBase cls
      Nothing -> "07"

allEntriesAsJSON :: Handler [JSON.Value]
allEntriesAsJSON = do
  conn <- pgsql
  res <- liftIO $ O.runSelect conn sql
  mapM (uncurry3 entryToJson) res
  where
    sql :: O.Select (O.Field O.SqlUuid, O.Field O.SqlText, O.Field O.SqlText)
    sql = do
      (i_, name_, _) <- O.selectTable entriesTable
      (_, class_, entity_i_, sub_, query_) <- O.selectTable entitiesTable
      O.where_ $ (i_ .== entity_i_) .&& (O.isNull sub_) .&& (O.isNull query_)
      return (i_, name_, class_)
    uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
    uncurry3 f (x, y, z) = f x y z

allEntriesJSON :: Handler JSON.Value
allEntriesJSON = JSON.toJSONList <$> allEntriesAsJSON

getAllEntriesR :: Handler TypedContent
getAllEntriesR = selectRep $ do
  provideRep allEntriesJSON
  provideRep $ defaultLayout widget
  where
    widget = do
      header HSEntries
      setTitle "Jump to entry"
      Rcs.fuzzy
      Rcs.entrySelect
      [whamlet|<h1>Jump to entry|]
      [whamlet|<input type="test" autocomplete="off" #select-query>|]
      [whamlet|
<fieldset #select-results>
  <legend> Results
  <div>
    <ul #select-list>
|]
