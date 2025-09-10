module Global where

import Lang
import Common
import Data.Map (Map)
import qualified Data.Map as Map


data Profile = Prof {
  steps :: Int,
  operations :: Int,
  maxStackSize :: Int,
  closures :: Int
} deriving Show 

data GlEnv = GlEnv {
  inter :: Bool,        -- ^ True, if we are in interactive mode.
  lfile :: String,      -- ^ Last loaded file.
  cantDecl :: Int,      -- ^ Number of declarations since last load
  profile :: Profile,   -- ^ Profiling information
  accounts :: Map String (Map String Int) -- ^ Account balances: account -> coin -> amount
} deriving Show


data Mode =
    Interactive
  | Eval

data Conf = Conf {
    opt :: Bool,          -- ^ True, if optimizations are enabled.
    modo :: Mode,
    profiling :: Bool
}

initialProfile :: Profile
initialProfile = Prof 0 0 0 0

-- | Initial state value
initialEnv :: GlEnv
initialEnv = GlEnv False "" 0 initialProfile Map.empty