{-# LANGUAGE ScopedTypeVariables #-}

module Korrvigs.Web.Home where

import Control.Lens
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import Korrvigs.Web.Login (logWrap)
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import Opaleye
import Yesod
import Prelude hiding (not)

getHomeR :: Handler Html
getHomeR = do
  entries :: [(EntryRow, Maybe Text)] <-
    rSelect $ limit 10 $ orderBy (descNullsLast (^. _1 . sqlEntryDate)) $ do
      entry <- selectTable entriesTable
      mtdt <- selectTable entriesMetadataTable
      where_ $ (mtdt ^. sqlEntry) .== (entry ^. sqlEntryName)
      where_ $ mtdt ^. sqlKey .== sqlStrictText "title"
      let titleText = sqlJsonToText $ mtdt ^. sqlValue
      -- where_ $ not $ isNull $ entry ^. sqlEntryDate
      pure (entry, titleText)
  logWrap $
    defaultLayout
      [whamlet|
      <h1>Welcome to Korrvigs
      <ul>
        $forall (entry,title) <- entries
          <li>
            ^{htmlKind $ entry ^. sqlEntryKind}
            <a href=@{EntryR $ WId $ entry ^. sqlEntryName}>
              $maybe t <- title
                #{t}
              $nothing
                #{unId $ entry ^. sqlEntryName}
    |]
