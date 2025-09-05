{-# LANGUAGE UndecidableInstances #-}

module Korrvigs.Compute.Action where

import Control.Applicative
import Control.Lens hiding ((.=))
import qualified Crypto.Hash as Hash
import Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types
import qualified Data.Default as Def
import Data.List
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstanceInferrable)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Korrvigs.Compute.Builtin as Builtin
import Korrvigs.Compute.Declare
import Korrvigs.Entry
import Korrvigs.Utils.Crypto
import Opaleye

data Action
  = Builtin Id Builtin.Action
  | Cached CompType CompHash
  deriving (Eq, Show)

instance ToJSON Action where
  toJSON (Builtin i act) =
    object
      [ "kind" .= String "builtin",
        "target" .= unId i,
        "value" .= act
      ]
  toJSON (Cached tp hash) =
    object ["kind" .= String "cached", "type" .= tp, "hash" .= digestToText hash]

instance FromJSON Action where
  parseJSON = withObject "Action" $ \act -> do
    kd <- act .: "kind"
    case kd of
      String "builtin" -> Builtin . MkId <$> (act .: "target") <*> act .: "value"
      String "cached" -> Cached <$> act .: "type" <*> (maybe empty pure . digestFromText =<< act .: "hash")
      String str -> fail $ T.unpack $ "\"" <> str <> "\" is not a valid computation kind name"
      obj -> unexpected obj

actionData :: Action -> ActionData
actionData (Builtin i blt) = Builtin.actionData blt & adatDeps . depEntries %~ (i :)
actionData (Cached tp hash) = ActionData tp Def.def $ Just hash

data CompRowImpl a b c = CompRow
  { _sqlCompEntry :: a,
    _sqlCompName :: b,
    _sqlCompAction :: c
  }

makeLenses ''CompRowImpl
$(makeAdaptorAndInstanceInferrable "pCompRow" ''CompRowImpl)

type CompRow = CompRowImpl Int Text Action

type CompRowSQL = CompRowImpl (Field SqlInt4) (Field SqlText) (Field SqlJsonb)

instance DefaultFromField SqlJsonb Action where
  defaultFromField = extract . fromJSON <$> defaultFromField
    where
      extract (Success x) = x
      extract _ = error "Invalid action in database"

instance Default ToFields CompRow CompRowSQL where
  def = pCompRow $ CompRow def def (lmap toJSON def)

computationsTable :: Table CompRowSQL CompRowSQL
computationsTable =
  table "computations" $
    pCompRow $
      CompRow
        (tableField "entry")
        (tableField "name")
        (tableField "action")

selComp :: Field SqlInt4 -> Text -> Select CompRowSQL
selComp i nm = do
  cmp <- selectTable computationsTable
  where_ $ cmp ^. sqlCompEntry .== i
  where_ $ cmp ^. sqlCompName .== sqlStrictText nm
  pure cmp

hashAction :: [CompHash] -> Action -> CompHash
hashAction deps (Builtin i blt) = Hash.hashlazy $ LEnc.encodeUtf8 $ encodeToLazyText v
  where
    v =
      object
        [ "deps" .= sort (digestToText <$> deps),
          "action" .= blt,
          "target" .= unId i,
          "tag" .= T.pack "builtin"
        ]
hashAction _ (Cached _ hash) = hash
