module Korrvigs.Web.Entry.Event (content, embed) where

import Control.Lens
import Data.Time.Format
import Korrvigs.Entry
import Korrvigs.Event
import Korrvigs.Event.ICalendar
import Korrvigs.Utils.Time
import Korrvigs.Web.Backend
import Yesod hiding (joinPath)

embed :: Int -> Event -> Handler Widget
embed _ event = do
  path <- eventPath event
  parsed <- liftIO $ parseICalFile path
  case parsed of
    Left err ->
      pure
        [whamlet|
        <p>
          Failed to load #{path}:
          <code>
            #{err}
      |]
    Right ical -> case ical ^. icEvent of
      Nothing -> undefined
      Just ievent -> do
        let cal = event ^. eventCalendar
        let location = ievent ^. iceLocation
        let startSpec = ievent ^. iceStart
        let startTime = resolveICalTime ical <$> startSpec
        let timeFormat = "%Y %B %e (%a) %R %EZ"
        let fmtTime = formatTime defaultTimeLocale timeFormat
        let endTime = case (startTime, ievent ^. iceDuration) of
              (Just start, Just dur) ->
                Just $ addNominal dur start
              _ -> resolveICalTime ical <$> ievent ^. iceEnd
        let description = ievent ^. iceDescription
        let transp = ievent ^. iceTransparent
        let summary = ievent ^. iceSummary
        pure
          [whamlet|
          <table>
            <tr>
              <td>Calendar
              <td>#{cal}
            $maybe s <- summary
              <tr>
                <td>Summary
                <td>#{s}
            $maybe loc <- location
              <tr>
                <td>Location
                <td>#{loc}
            $maybe start <- startTime
              <tr>
                <td>Start
                <td>#{fmtTime start}
            $maybe end <- endTime
              <tr>
                <td>End
                <td>#{fmtTime end}
            $maybe desc <- description
              <tr>
                <td>Description
                <td>
                  <p style="white-space: pre-line;">
                    #{desc}
            <tr>
              <td>Opaque
              $if transp
                <td>Free
              $else
                <td>Busy
        |]

content :: Event -> Handler Widget
content = embed 0
