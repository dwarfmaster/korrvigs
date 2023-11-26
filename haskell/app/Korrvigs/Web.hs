{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.Web (Korrvigs (..)) where

import Control.Monad ((>=>))
import Data.ByteString (readFile)
import Data.Text (Text)
import qualified Korrvigs.Classes as Cls
import Korrvigs.Definition
import qualified Korrvigs.Relations as Rels
import Korrvigs.Web.Backend
import Korrvigs.Web.DB
import Korrvigs.Web.Entry (getAllEntriesR, processEntry, renderEntry)
import Korrvigs.Web.Entry.Notes (noteEditor)
import Korrvigs.Web.Header
import Korrvigs.Web.Method
import qualified Korrvigs.Web.Query as Query
import Korrvigs.Web.Routes
import qualified Korrvigs.Web.Sub as Sub
import Korrvigs.Web.UUID (UUID (UUID))
import Yesod
import Prelude hiding (readFile)

mkYesodDispatch "Korrvigs" korrvigsRoutes

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  header HSHome
  [whamlet|Hello from korrvigs!|]

getEntryR :: UUID -> Handler Html
getEntryR (UUID uuid) =
  findEntry uuid
    >>= maybe notFound (renderEntry >=> addHeaderM HSEntries >=> defaultLayout)

postEntryR :: UUID -> Handler Html
postEntryR (UUID uuid) = findEntry uuid >>= maybe notFound (processEntry >=> defaultLayout)

getEntryEditR :: UUID -> Handler TypedContent
getEntryEditR (UUID uuid) = noteEditor methodGet uuid defaultLayout

postEntryEditR :: UUID -> Handler TypedContent
postEntryEditR (UUID uuid) = noteEditor methodPost uuid defaultLayout

getEntryQueryR :: UUID -> Text -> Handler Html
getEntryQueryR (UUID uuid) query = generateForQuery $ EntityRef uuid Nothing (Just query)

getEntrySubR :: UUID -> Text -> Handler Html
getEntrySubR (UUID uuid) sub =
  findEntry uuid
    >>= maybe notFound (flip Sub.widget sub >=> addHeaderM HSEntries >=> defaultLayout)

getEntrySubContentR :: UUID -> Text -> Handler TypedContent
getEntrySubContentR (UUID uuid) sub =
  findEntry uuid >>= \case
    Nothing -> notFound
    Just entry -> do
      root <- korrRoot
      let path = entrySubPath root entry sub
      let mime = Sub.mimeFor sub
      file <- liftIO $ readFile path
      pure $ TypedContent mime $ toContent file

getEntrySubQueryR :: UUID -> Text -> Text -> Handler Html
getEntrySubQueryR (UUID uuid) sub query = generateForQuery $ EntityRef uuid (Just sub) (Just query)

generateForQuery :: EntityRef -> Handler Html
generateForQuery ref =
  findEntity ref >>= maybe notFound (Query.widget >=> addHeaderM HSEntries >=> defaultLayout)

getGenerateClassesR :: Handler Text
getGenerateClassesR = pgsql >>= Cls.generateClassHs

getGenerateRelsSqlR :: Handler Text
getGenerateRelsSqlR = do
  conn <- pgsql
  root <- korrRoot
  Rels.generateRelationsSql conn root

getGenerateRelsHsR :: Handler Text
getGenerateRelsHsR = do
  conn <- pgsql
  root <- korrRoot
  Rels.generateRelationsHs conn root
