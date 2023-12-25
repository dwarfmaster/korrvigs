module Korrvigs.Web.Entry.OntologyRelation (newOntologyRelation, schemaWidget) where

import Control.Monad (void)
import Data.Aeson (encode)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (writeFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Database.PostgreSQL.Simple.Transaction (withTransaction)
import Korrvigs.Classes
import Korrvigs.Classes.Sync (mkMdName)
import Korrvigs.Definition
import Korrvigs.Entry
import Korrvigs.Relations (Type (..), parseDhall)
import qualified Korrvigs.Tree as Tree
import Korrvigs.Web.Backend
import Korrvigs.Web.Entry.Types
import qualified Korrvigs.Web.UUID as U
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Text.Pandoc.Builder (Blocks, blockQuote, para, rawBlock, simpleCell, table, text, toList)
import Text.Pandoc.Definition (Alignment (AlignCenter), Caption (..), ColWidth (ColWidthDefault), Row (..), RowHeadColumns (..), TableBody (..), TableFoot (..), TableHead (..))
import Yesod
import Prelude hiding (writeFile)

newOntologyRelation :: Text -> Text -> Text -> Handler a
newOntologyRelation nm desc dhall = do
  conn <- pgsql
  root <- korrRoot
  entry <- liftIO $ withSystemTempDirectory "korrvigs" $ \temp -> do
    let path = temp </> "relation.dhall"
    writeFile path dhall
    withTransaction conn $ do
      entry <- newEntry conn root OntologyRelation nm $ mkMdName nm
      void $ newEntity conn root entry File (Just path) Nothing
      return entry
  Tree.writeNotes root entry desc
  redirect $ EntryR $ U.UUID $ entry_id entry

schemaWidget :: Entry -> Handler WidgetMap
schemaWidget entry = do
  root <- korrRoot
  let path = subPath root (entry_id entry) "relation.dhall"
  res <- liftIO $ parseDhall path
  M.singleton "Schema" . Left <$> case res of
    Left err -> pure $ para (text "Error:") <> blockQuote (para $ text $ T.pack err)
    Right types -> do
      rendered <- mapM (\(nm, tp) -> (T.pack nm,) <$> renderTp tp) types
      let at = ("", [], [])
      let textCell = simpleCell . para . text
      pure $
        table
          ( let c = text "Relation arguments"
             in Caption (Just $ toList c) $ toList $ para c
          )
          [(AlignCenter, ColWidthDefault), (AlignCenter, ColWidthDefault)]
          (TableHead at [Row at [textCell "Argument", textCell "Type"]])
          [TableBody at (RowHeadColumns 0) [] $ (\(nm, tp) -> Row at [textCell nm, simpleCell tp]) <$> rendered]
          (TableFoot at [])
  where
    encodeText :: ToJSON a => [(Text, a)] -> Text
    encodeText = toStrict . decodeUtf8 . encode . toJSON . M.fromList
    render :: Text -> Blocks
    render txt =
      rawBlock "class" $
        encodeText
          [ ("text", txt),
            ("color", "var(--base05)")
          ]
    renderTp :: Type -> Handler Blocks
    renderTp TText = pure $ render "Text"
    renderTp TUUID = pure $ render "UUID"
    renderTp (TClass cls) = do
      conn <- pgsql
      clsEntry <- lookupEntryByName' conn OntologyClass $ name cls
      pure $
        rawBlock "class" $
          encodeText
            [ ("class", name cls),
              ("uuid", T.pack $ show $ entry_id clsEntry)
            ]
