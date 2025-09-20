{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Kind.Def where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import qualified Data.Text as T

data Kind
  = Note
  | Link
  | File
  | Event
  | Calendar
  | Syndicate
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Singletons
data NoteK = NoteK

data LinkK = LinkK

data FileK = FileK

data EventK = EventK

data CalendarK = CalendarK

makeLenses ''Kind

-- JSON
instance ToJSON Kind where
  toJSON Note = "note"
  toJSON Link = "link"
  toJSON File = "file"
  toJSON Event = "event"
  toJSON Calendar = "calendar"
  toJSON Syndicate = "syndicate"

instance FromJSON Kind where
  parseJSON = withText "Kind" $ \case
    "note" -> pure Note
    "link" -> pure Link
    "file" -> pure File
    "event" -> pure Event
    "calendar" -> pure Calendar
    "syndicate" -> pure Syndicate
    s -> fail $ T.unpack s <> " is not a valid kind"
