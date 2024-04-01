{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Link.SQL where

import Control.Lens
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad
import Opaleye

data LinkRowImpl a b c d = LinkRow
  { _sqlLinkName :: a,
    _sqlLinkProtocol :: b,
    _sqlLinkRef :: c,
    _sqlLinkFile :: d
  }

makeLenses ''LinkRowImpl
$(makeAdaptorAndInstanceInferrable "pLinkRow" ''LinkRowImpl)

type LinkRow = LinkRowImpl Text Text Text FilePath

mkLinkRow :: Text -> Text -> Text -> FilePath -> LinkRow
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

dLoadImpl :: MonadKorrvigs m => Id -> ((Entry -> Link) -> Entry) -> m (Maybe Entry)
dLoadImpl i cstr = do
  sel <- rSelectOne $ do
    lrow <- selectTable linksTable
    where_ $ lrow ^. sqlLinkName .== sqlStrictText (unId i)
    pure lrow
  case (sel :: Maybe LinkRow) of
    Nothing -> pure Nothing
    Just lrow -> do
      let lnk entry =
            MkLink
              { _linkEntry = entry,
                _linkProtocol = lrow ^. sqlLinkProtocol,
                _linkRef = lrow ^. sqlLinkRef,
                _linkPath = lrow ^. sqlLinkFile
              }
      pure $ Just $ cstr lnk
