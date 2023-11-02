module Korrvigs.Classes.Colors where

import Data.Hashable (hash)
import Korrvigs.Classes
import System.Random (mkStdGen, randomR)
import Text.Printf

-- The generated integer is in [0,7]
classColor :: Class -> Int
classColor = fst . randomR (0, 7) . mkStdGen . hash . name

-- Return the base in base16 to color the class with, so 08, 09, 0A...
classBase :: Class -> String
classBase = printf "%02X" . (+ 8) . classColor
