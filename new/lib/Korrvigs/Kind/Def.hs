{-# LANGUAGE TemplateHaskell #-}

module Korrvigs.Kind.Def where

import Control.Lens.TH (makeLenses)

data Kind
  = Note
  | Link
  deriving (Eq, Ord, Show, Enum)

makeLenses ''Kind
