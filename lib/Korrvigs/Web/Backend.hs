module Korrvigs.Web.Backend where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import qualified Korrvigs.Actions as Actions
import Korrvigs.Monad
import Korrvigs.Web.Routes
import qualified Web.ClientSession as CS
import Yesod

data WebData = WebData
  { web_connection :: Connection,
    web_root :: FilePath,
    web_theme :: Int -> Text -- Base16 theme
  }

mkYesodData "WebData" korrvigsRoutes

getBase :: Handler (Int -> Text)
getBase = web_theme <$> getYesod

instance Yesod WebData where
  jsLoader _ = BottomOfBody
  makeSessionBackend _ =
    strictSameSiteSessions $
      Just <$> defaultClientSessionBackend (24 * 60) CS.defaultKeyFile

instance RenderMessage WebData FormMessage where
  renderMessage _ _ = defaultFormMessage

instance MonadKorrvigs Handler where
  pgSQL = getsYesod web_connection
  root = getsYesod web_root
  load = Actions.load
  remove = Actions.remove
  dispatchRemove = Actions.dispatchRemove
  removeDB = Actions.removeDB
  dispatchRemoveDB = Actions.dispatchRemoveDB
  sync = Actions.sync
