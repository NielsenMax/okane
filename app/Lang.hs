{-# LANGUAGE DeriveFunctor #-}

module Lang where

import Common

data Perc = Perc Int | Rat Int Int deriving (Show, Eq)

-- | Source specification - either single account or multiple accounts with connector
data Src var
  = SSingle var                    -- Single account
  | SMultiple [SrcConnector var]   -- Multiple accounts with connector
  deriving (Show, Functor, Eq)

-- | Connector for multiple sources
data SrcConnector var
  = SConnPerc Perc (Src var)       -- Percentage of a source
  | SConnMax Int (Src var)         -- Max amount from a source
  | SConnRem (Src var)             -- Remaining from a source
  deriving (Show, Functor, Eq)

-- | Destination specification - either single account or multiple accounts with connector
data Dst var
  = DSingle var                    -- Single account
  | DMultiple [DstConnector var]   -- Multiple accounts with connector
  deriving (Show, Functor, Eq)

-- | Connector for multiple destinations
data DstConnector var
  = DConnPerc Perc (Dst var)       -- Percentage of a destination
  | DConnRem (Dst var)             -- Remaining to a destination
  deriving (Show, Functor, Eq)

data Coin = Coin String Int deriving (Show, Eq)

-- | AST of terms with positions
data STm var
  = SSend (Src var) Coin (Dst var)
  | STrx [STm var]
  | SAccount String String Int
  deriving (Show, Eq)

-- | Terms with positions
type STerm = STm String

-- | Declarations
data Decl a
  = Decl
      { declPos :: Pos,
        declName :: Name,
        declBody :: a
      }
  deriving (Show, Functor, Eq)