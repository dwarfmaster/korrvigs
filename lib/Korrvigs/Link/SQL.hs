{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Link.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import GHC.Int (Int64)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Class
import Korrvigs.Monad.Utils
import Opaleye

data LinkRowImpl a b c d = LinkRow
  { _sqlLinkName :: a,
    _sqlLinkProtocol :: b,
    _sqlLinkRef :: c,
    _sqlLinkFile :: d
  }

makeLenses ''LinkRowImpl
$(makeAdaptorAndInstanceInferrable "pLinkRow" ''LinkRowImpl)

type LinkRow = LinkRowImpl Id Text Text FilePath

mkLinkRow :: Id -> Text -> Text -> FilePath -> LinkRow
mkLinkRow = LinkRow

type LinkRowSQL = LinkRowImpl (Field SqlText) (Field SqlText) (Field SqlText) (Field SqlText)

instance Default ToFields LinkRow LinkRowSQL where
  def = pLinkRow $ LinkRow def def def def

linksTable :: Table LinkRowSQL LinkRowSQL
linksTable =
  table "links" $
    pLinkRow $
      LinkRow
        (nameKindField Link)
        (tableField "protocol")
        (tableField "ref")
        (tableField "file")

linkFromRow :: LinkRow -> Entry -> Link
linkFromRow row entry =
  MkLink
    { _linkEntry = entry,
      _linkProtocol = row ^. sqlLinkProtocol,
      _linkRef = row ^. sqlLinkRef,
      _linkPath = row ^. sqlLinkFile
    }

sqlLoad :: (MonadKorrvigs m) => Id -> ((Entry -> Link) -> Entry) -> m (Maybe Entry)
sqlLoad = genSqlLoad linksTable (view sqlLinkName) linkFromRow

sqlRemove :: Id -> [Delete Int64]
sqlRemove = genSqlRemove linksTable $ view sqlLinkName
