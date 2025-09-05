{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Korrvigs.File.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import qualified Data.Text.Encoding as Enc
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.Utils
import Korrvigs.Utils.Opaleye (makeSqlMapper)
import Network.Mime
import Opaleye
import Opaleye.Experimental.Enum

-- FileStatus
data SqlFileStatus

toSqlFS :: FileStatus -> String
toSqlFS FilePlain = "fileplain"
toSqlFS FilePresent = "filepresent"
toSqlFS FileAbsent = "fileabsent"

sqlFSMapper :: EnumMapper SqlFileStatus FileStatus
sqlFSMapper = makeSqlMapper "filestatus" toSqlFS

instance DefaultFromField SqlFileStatus FileStatus where
  defaultFromField = enumFromField sqlFSMapper

instance Default ToFields FileStatus (Field SqlFileStatus) where
  def = enumToFields sqlFSMapper

sqlFS :: FileStatus -> Field SqlFileStatus
sqlFS = toFields

-- Mime type
instance DefaultFromField SqlText MimeType where
  defaultFromField = Enc.encodeUtf8 <$> defaultFromField

instance Default ToFields MimeType (Field SqlText) where
  def = dimap Enc.decodeUtf8 id def

-- Files table
data FileRowImpl a b c d e = FileRow
  { _sqlFileId :: a,
    _sqlFilePath :: b,
    _sqlFileMeta :: c,
    _sqlFileStatus :: d,
    _sqlFileMime :: e
  }

makeLenses ''FileRowImpl
$(makeAdaptorAndInstanceInferrable "pFileRow" ''FileRowImpl)

type FileRow = FileRowImpl Int FilePath FilePath FileStatus MimeType

mkFileRow :: Int -> FilePath -> FilePath -> FileStatus -> MimeType -> FileRow
mkFileRow = FileRow

type FileRowSQL = FileRowImpl (Field SqlInt4) (Field SqlText) (Field SqlText) (Field SqlFileStatus) (Field SqlText)

instance Default ToFields FileRow FileRowSQL where
  def = pFileRow $ FileRow def def def def def

filesTable :: Table FileRowSQL FileRowSQL
filesTable =
  table "files" $
    pFileRow $
      FileRow
        (nameKindField File)
        (tableField "path")
        (tableField "meta")
        (tableField "status")
        (tableField "mime")

fileFromRow :: FileRow -> Entry -> File
fileFromRow frow entry =
  MkFile
    { _fileEntry = entry,
      _filePath = frow ^. sqlFilePath,
      _fileMeta = frow ^. sqlFileMeta,
      _fileStatus = frow ^. sqlFileStatus,
      _fileMime = frow ^. sqlFileMime
    }

sqlLoad :: (MonadKorrvigs m) => Int -> ((Entry -> File) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad filesTable (view sqlFileId) fileFromRow

sqlRemove :: Int -> [Delete Int64]
sqlRemove = genSqlRemove filesTable $ view sqlFileId
