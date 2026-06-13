module Korrvigs.Web.Actions.Date where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format
import Data.Time.LocalTime
import Korrvigs.Entry
import Korrvigs.Kind
import Korrvigs.Monad.Metadata
import Korrvigs.Web.Actions.Defs
import Korrvigs.Web.Backend
import Yesod

dateTarget :: ActionTarget -> ActionCond
dateTarget (TargetEntry entry) | entry ^. kind /= Event = ActCondAlways
dateTarget _ = ActCondNever

dateForm :: AForm Handler (LocalTime, Maybe Text)
dateForm =
  (,)
    <$> areq datetimeLocalField "Start" Nothing
    <*> aopt textField "End" Nothing

dateTitle :: ActionTarget -> Text
dateTitle = const "Set date"

runDate :: (LocalTime, Maybe Text) -> ActionTarget -> Handler ActionReaction
runDate (start, dur) (TargetEntry entry) = do
  tz <- liftIO getCurrentTimeZone
  let startDate = ZonedTime start tz
  let duration = parseTimeM True defaultTimeLocale "%d-%h:%m" . T.unpack =<< dur
  updateDate entry (Just startDate)
  forM_ duration $ updateDuration entry . Just
  pure $ def & reactMsg ?~ [shamlet|<p>Set date|]
runDate _ _ = pure def
