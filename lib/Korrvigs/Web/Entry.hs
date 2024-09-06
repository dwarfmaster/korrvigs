module Korrvigs.Web.Entry (getEntryR, postEntryR) where

import Control.Lens
import Control.Monad
import qualified Data.Aeson.Encoding as VEnc
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as Enc
import Korrvigs.Entry
import Korrvigs.Monad
import Korrvigs.Utils.JSON
import Korrvigs.Web.Backend
import Korrvigs.Web.Login
import qualified Korrvigs.Web.Ressources as Rcs
import Korrvigs.Web.Routes
import Korrvigs.Web.Utils
import Opaleye
import Yesod

-- An entry page is constitued of the following parts
-- The entry title (if any) + the entry name
-- A link to download the entry
-- The metadata in a foldable blocks, with edition
-- The backlinks,parents and subs
-- The entry content

titleWidget :: Entry -> Handler Widget
titleWidget entry = do
  title :: Maybe Text <- fmap join $ rSelectOne $ do
    mtdt <- selectTable entriesMetadataTable
    where_ $ (mtdt ^. sqlEntry) .== sqlId (entry ^. name)
    where_ $ mtdt ^. sqlKey .== sqlStrictText "title"
    pure $ sqlJsonToText $ toNullable $ mtdt ^. sqlValue
  pure
    [whamlet|
    ^{htmlKind $ entry ^. kind}
    <span .download-button>
      <a href=@{EntryDownloadR $ WId $ entry ^. name}>
        ⬇
    <h1>
      $maybe t <- title
        #{t}
      <span .entry-name>
        (#{unId $ entry ^. name})
  |]

mtdtWidget :: Entry -> Handler Widget
mtdtWidget entry = do
  pure
    [whamlet|
  <details>
    <summary>Metadata
    <table>
      <tr>
        <th>Key
        <th>Value
        <th>Read Only
      $forall (key,val) <- M.toList (entry ^. metadata)
        <tr>
          <th>#{key}
          <th>
            #{prepareMtdtValue $ val ^. metaValue}
          <th>
            $if val ^. metaReadOnly
              True
            $else
              False
  |]
  where
    prepareMtdtValue :: Value -> LT.Text
    prepareMtdtValue val =
      let txt = Enc.decodeUtf8 $ VEnc.encodingToLazyByteString $ VEnc.value val
       in if LT.length txt < 50
            then txt
            else LT.take 47 txt <> "..."

entryWidget :: Entry -> Handler Widget
entryWidget entry = do
  title <- titleWidget entry
  mtdt <- mtdtWidget entry
  pure $ do
    Rcs.entryStyle
    title
    mtdt

getEntryR :: WebId -> Handler Html
getEntryR (WId i) =
  load i >>= \case
    Just entry -> entryWidget entry >>= logWrap . defaultLayout
    Nothing -> notFound

postEntryR :: WebId -> Handler Html
postEntryR (WId _) = logWrap undefined
