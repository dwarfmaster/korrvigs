module Main where

import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson (toJSON)
import Data.ByteString (ByteString)
import Data.Profunctor.Product.Default ()
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import qualified Korrvigs.Actions as Actions
import Korrvigs.Link
import Korrvigs.Monad
import Prelude hiding (putStr)

data KorrState = KState
  { _korrConnection :: Connection,
    _korrRoot :: FilePath
  }

makeLenses ''KorrState

type KorrM = ExceptT KorrvigsError (ReaderT KorrState IO)

instance MonadKorrvigs KorrM where
  pgSQL = view korrConnection
  root = view korrRoot
  load = Actions.load
  remove = Actions.remove
  removeDB = Actions.removeDB
  sync = Actions.sync

runKorrM :: ByteString -> KorrM a -> IO (Either KorrvigsError a)
runKorrM connSpec action = do
  conn <- connectPostgreSQL connSpec
  r <- runReaderT (runExceptT action) $ KState conn "/tmp/korrvigs"
  close conn
  pure r

link1 :: LinkMaker
link1 =
  lmk "dwarfmaster (Luc Chabassier)" "https" "github.com/dwarfmaster"
    & lkMtdt . at "date" ?~ toJSON ("2024-04-22" :: Text)

main :: IO ()
main =
  print
    =<< runKorrM
      "dbname='korrvigs_new'"
      ( do
          sync
          -- void $ newLink link1
      )
