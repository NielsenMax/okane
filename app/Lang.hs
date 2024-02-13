{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE GADTs #-}

module Lang where

data Perc = Perc Int | Rat Int Int deriving (Show)

data Src var
  = SAcc var
  | SPerc Perc (Srcs var)
  | SMax Int (Srcs var)
  | SRem (Srcs var)
  deriving (Show, Functor)

type Srcs var = [Src var]

data Dst var
  = DAcc var
  | DPerc Perc (Dsts var)
  | DRem (Dsts var)
  deriving (Show, Functor)

type Dsts var = [Dst var]

data Coin = Coin String Int

data Ty
  = TS
  | TD
  | TC

data Param var
  = PS (Srcs var)
  | PD (Dsts var)
  | PC Coin 

data STm var
  = SSend (Srcs var) Coin (Dsts var)
  | SFun String (var, Ty) [STm var]
  | STrx [STm var]
  | SRun String [Param var]

type STerm = STm String