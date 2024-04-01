{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Kind.Def where

import Control.Lens.TH (makeLenses)

data Kind
  = Note
  | Link
  deriving (Eq, Ord, Show, Enum)

-- Singletons
data NoteK = NoteK

data LinkK = LinkK

makeLenses ''Kind
