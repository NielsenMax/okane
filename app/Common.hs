module Common where

import Data.List.Extra (nubSort)

type Pos = Int
type Name = String

-- | Position constructors
noPos :: Pos
noPos = 0