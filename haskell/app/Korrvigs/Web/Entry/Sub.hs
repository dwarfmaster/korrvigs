module Korrvigs.Web.Entry.Sub where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Korrvigs.Classes
import Korrvigs.Definition
import Korrvigs.Pandoc (renderUrl)
import Korrvigs.Schema
import Korrvigs.Web.Backend
import Opaleye ((.&&), (.==))
import qualified Opaleye as O
import Text.Pandoc.Builder (Blocks, bulletList, link, para, text)
import Yesod hiding (Entity)

make :: Entry -> Handler (Map String (Either Blocks Widget))
make entry = do
  conn <- pgsql
  subs <- liftIO $ O.runSelect conn $ do
    (_, cls_, uuid_, sub_, query_) <- O.selectTable entitiesTable
    O.where_ $ uuid_ .== O.sqlUUID (entry_id entry)
    O.where_ $ O.isNull query_ .&& cls_ .== O.sqlStrictText (name File)
    pure sub_
  pure $ maybe M.empty (M.singleton "Subs" . Left) $ build entry $ catMaybes subs

build :: Entry -> [Text] -> Maybe Blocks
build _ [] = Nothing
build entry subs =
  Just $ bulletList $ (\sub -> para $ link (renderUrl $ EntityRef (entry_id entry) (Just sub) Nothing) sub $ text sub) <$> subs
