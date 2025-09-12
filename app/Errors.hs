module Errors (Error(..), OkaneError(..), Pos(..)) where

import Common

-- | Error type for the Okane DSL
data OkaneError
  = ErrPos Pos String
  | InsufficientFunds String String Int Int  -- account, coin, required, available
  | UnknownAccount String
  | DivisionByZero
  | InvalidPercentage Int
  | InvalidPercentageSum Int Int  -- actual percentage, expected percentage
  | ParseError String
  deriving (Show)

-- | Alias for compatibility
type Error = OkaneError