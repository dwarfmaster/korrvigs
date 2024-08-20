{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Link.JSON where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.Map (Map)
import Data.Text (Text)
import Korrvigs.Monad
import Korrvigs.Utils.DateTree (DateTreeType, dtMonth, dtYear)
import System.FilePath
import Prelude hiding (readFile, writeFile)

data LinkJSON = LinkJSON
  { _lkjsProtocol :: Text,
    _lkjsLink :: Text,
    _lkjsMetadata :: Map Text Value,
    _lkjsParents :: [Text]
  }

makeLenses ''LinkJSON

instance FromJSON LinkJSON where
  parseJSON (Object v) =
    LinkJSON
      <$> v
        .: "protocol"
      <*> v
        .: "link"
      <*> v
        .: "metadata"
      <*> v
        .: "parents"
  parseJSON invalid =
    prependFailure "parsing link failed, " $ typeMismatch "Object" invalid

instance ToJSON LinkJSON where
  toJSON (LinkJSON prot lk mtdt prts) =
    object
      [ "protocol" .= prot,
        "link" .= lk,
        "metadata" .= mtdt,
        "parents" .= prts
      ]

linkJSONPath :: (MonadKorrvigs m) => m FilePath
linkJSONPath = joinPath . (: ["links"]) <$> root

linkJSONTreeType :: DateTreeType
linkJSONTreeType = def & dtYear .~ True & dtMonth .~ True
