module Korrvigs.Web.Header where

import Data.Text (Text)
import Korrvigs.Web.Backend
import qualified Korrvigs.Web.Ressources as Rcs

data HeaderSection
  = HSHome
  | HSEntries
  deriving (Eq, Show, Ord, Enum, Bounded)

headerRoute :: HeaderSection -> Route Korrvigs
headerRoute HSHome = HomeR
headerRoute HSEntries = AllEntriesR

headerPrint :: HeaderSection -> Text
headerPrint HSHome = "Home"
headerPrint HSEntries = "Entries"

header :: HeaderSection -> Widget
header active = Rcs.header pages
  where
    pages :: [(Bool, Text, Route Korrvigs)]
    pages =
      (\hd -> (hd == active, headerPrint hd, headerRoute hd))
        <$> [minBound .. maxBound]

addHeader :: HeaderSection -> Widget -> Widget
addHeader = (>>) . header

addHeaderM :: HeaderSection -> Widget -> Handler Widget
addHeaderM = (pure .) . addHeader
