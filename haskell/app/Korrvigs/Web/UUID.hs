module Korrvigs.Web.UUID where

import qualified Data.UUID as U
import Yesod

newtype UUID = UUID U.UUID
  deriving (Eq, Show, Read)

instance PathPiece UUID where
  toPathPiece (UUID uuid) = U.toText uuid
  fromPathPiece s = UUID <$> U.fromText s
