module Korrvigs.Link.Sync where

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Aeson (eitherDecode, toJSON)
import Data.ByteString.Lazy (readFile)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Link.JSON
import Korrvigs.Link.SQL
import Korrvigs.Metadata
import Korrvigs.Monad
import Korrvigs.Utils.DateTree (listFiles)
import Opaleye
import System.Directory (removeFile)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

linkIdFromPath :: FilePath -> Id
linkIdFromPath = MkId . T.pack . takeBaseName

syncLink :: MonadKorrvigs m => FilePath -> m ()
syncLink path = do
  let i = linkIdFromPath path
  remove i
  json <- liftIO (eitherDecode <$> readFile path) >>= throwEither (KCantLoad i . T.pack)
  let mtdt = json ^. lkjsMetadata
  let geom = mtdtGeometry mtdt
  let (tm, dur) = mtdtDate mtdt
  let erow = EntryRow (unId i) Link tm dur geom Nothing (Just $ toJSON mtdt) :: EntryRow
  let lrow = LinkRow (unId i) (json ^. lkjsProtocol) (json ^. lkjsLink) path :: LinkRow
  atomicSQL $ \conn -> do
    void $
      runInsert conn $
        Insert
          { iTable = entriesTable,
            iRows = [toFields erow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
    void $
      runInsert conn $
        Insert
          { iTable = linksTable,
            iRows = [toFields lrow],
            iReturning = rCount,
            iOnConflict = Just doNothing
          }
  pure ()

allJSONs :: MonadKorrvigs m => m [FilePath]
allJSONs = do
  rt <- linkJSONPath
  let dtt = linkJSONTreeType
  files <- listFiles rt dtt
  pure $ (^. _1) <$> files

dListImpl :: MonadKorrvigs m => m (Set FilePath)
dListImpl = S.fromList <$> allJSONs

dGetIdImpl :: FilePath -> Id
dGetIdImpl = linkIdFromPath

dSyncImpl :: MonadKorrvigs m => f Link -> m ()
dSyncImpl _ = allJSONs >>= mapM_ syncLink

dSyncOneImpl :: MonadKorrvigs m => FilePath -> m ()
dSyncOneImpl = syncLink

dRemoveImpl :: MonadKorrvigs m => FilePath -> m ()
dRemoveImpl path = do
  let i = linkIdFromPath path
  atomicSQL $ \conn -> do
    removeFile path
    void $
      runDelete conn $
        Delete
          { dTable = linksTable,
            dWhere = \lrow -> lrow ^. sqlLinkName .== sqlStrictText (unId i),
            dReturning = rCount
          }
    void $
      runDelete conn $
        Delete
          { dTable = entriesTable,
            dWhere = \erow -> erow ^. sqlEntryName .== sqlStrictText (unId i),
            dReturning = rCount
          }
