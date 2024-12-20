{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Kind.Def where

import Control.Lens.TH (makeLenses)

data Kind
  = Note
  | Link
  | File
  | Event
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Singletons
data NoteK = NoteK

data LinkK = LinkK

data FileK = FileK

data EventK = EventK

makeLenses ''Kind
