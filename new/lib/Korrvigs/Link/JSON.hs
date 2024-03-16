{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Link.JSON where

import Control.Lens hiding ((.=))
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Encoding (encodingToLazyByteString)
import Data.Aeson.Types
import Data.ByteString.Lazy (readFile, writeFile)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Korrvigs.Entry
import Korrvigs.Monad
import System.FilePath
import Prelude hiding (readFile, writeFile)

data LinkJSON = LinkJSON
  { _lkjsId :: Text,
    _lkjsProtocol :: Text,
    _lkjsLink :: Text,
    _lkjsMetadata :: Map Text Value,
    _lkjsChildren :: [Text]
  }

makeLenses ''LinkJSON

instance FromJSON LinkJSON where
  parseJSON (Object v) =
    LinkJSON
      <$> v
      .: "id"
      <*> v
      .: "protocol"
      <*> v
      .: "link"
      <*> v
      .: "metadata"
      <*> v
      .: "children"
  parseJSON invalid =
    prependFailure "parsing link failed, " $ typeMismatch "Object" invalid

instance ToJSON LinkJSON where
  toJSON (LinkJSON i prot lk mtdt child) =
    object
      [ "id" .= i,
        "protocol" .= prot,
        "link" .= lk,
        "metadata" .= mtdt,
        "children" .= child
      ]

linkJSONPath :: MonadKorrvigs m => m FilePath
linkJSONPath = joinPath . (: ["links.json"]) <$> root

linkFromJSON :: LinkJSON -> Link
linkFromJSON js =
  let lnk =
        MkLink
          { _linkEntry =
              MkEntry
                { _name = MkId $ js ^. lkjsId,
                  -- TODO extract the following three from the metadata
                  _date = Nothing,
                  _duration = Nothing,
                  _geo = Nothing,
                  _metadata = js ^. lkjsMetadata,
                  _kind = LinkD lnk,
                  _children = Ref . MkId <$> js ^. lkjsChildren
                },
            _linkProtocol = js ^. lkjsProtocol,
            _linkRef = js ^. lkjsLink
          }
   in lnk

linkToJSON :: Link -> LinkJSON
linkToJSON lk =
  let entry = lk ^. linkEntry
   in LinkJSON
        { _lkjsId = unId $ entry ^. name,
          _lkjsProtocol = lk ^. linkProtocol,
          _lkjsLink = lk ^. linkRef,
          -- TODO Add to metadata the information for date/duration/geo
          _lkjsMetadata = entry ^. metadata,
          _lkjsChildren = []
        }

readLinksJSON :: MonadKorrvigs m => m (Either Text [LinkJSON])
readLinksJSON = do
  path <- linkJSONPath
  content <- liftIO $ readFile path
  pure $ case eitherDecode content of
    Right lks -> Right lks
    Left err -> Left $ T.pack err

writeLinksJSON :: MonadKorrvigs m => [LinkJSON] -> m ()
writeLinksJSON lks = do
  path <- linkJSONPath
  let content = encodingToLazyByteString $ toEncoding lks
  liftIO $ writeFile path content
