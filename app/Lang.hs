{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}

module Lang where

import Common

data Perc = Perc Int | Rat Int Int deriving (Show, Eq)

data Src var
  = SAcc var
  | SPerc Perc (Srcs var)
  | SMax Int (Srcs var)
  | SRem (Srcs var)
  deriving (Show, Functor, Eq)

type Srcs var = [Src var]

data Dst var
  = DAcc var
  | DPerc Perc (Dsts var)
  | DRem (Dsts var)
  deriving (Show, Functor, Eq)

type Dsts var = [Dst var]

data Coin = Coin String Int deriving (Show, Eq)

-- | AST of terms with positions
data STm var
  = SSend (Srcs var) Coin (Dsts var)
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