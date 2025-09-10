{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module MonadOkane
  ( Okane,
    runOkane,
    lookupAccount,
    addAccount,
    getAccounts,
    failPosOkane,
    failOkane,
    printOkane,
    MonadOkane,
    module Control.Monad.Except,
    module Control.Monad.State,
  )
where

import Control.Monad.Except
import Control.Monad.State
import Lang
import Global
import Errors
import Common (noPos)
import Data.Map (Map)
import qualified Data.Map as Map
import System.IO

-- * The MonadOkane class

-- | The MonadOkane class classifies monads with support for state management and errors
class (MonadIO m, MonadState GlEnv m, MonadError OkaneError m) => MonadOkane m where
  addStep :: m ()
  addOpp :: m ()
  addMaxStack :: Int -> m ()
  addClos :: m ()

-- Account and function lookup operations
lookupAccount :: (MonadOkane m) => String -> String -> m (Maybe Int)
lookupAccount account coin = do
  s <- get
  let accountMap = Map.findWithDefault Map.empty account (accounts s)
  return $ Map.lookup coin accountMap


addAccount :: (MonadOkane m) => String -> String -> Int -> m ()
addAccount account coin amount = do
  s <- get
  let accountMap = Map.findWithDefault Map.empty account (accounts s)
  let newAccountMap = if amount == 0 
                      then Map.delete coin accountMap
                      else Map.insert coin amount accountMap
  let newAccounts = Map.insert account newAccountMap (accounts s)
  put $ s { accounts = newAccounts }

getAccounts :: (MonadOkane m) => m (Map String (Map String Int))
getAccounts = gets accounts

failPosOkane :: (MonadOkane m) => Pos -> String -> m a
failPosOkane p s = throwError (ErrPos p s)

failOkane :: (MonadOkane m) => String -> m a
failOkane = failPosOkane noPos

printOkane :: (MonadOkane m) => String -> m ()
printOkane = liftIO . putStrLn

-- | The Okane type is a monad with state and error handling
type Okane = StateT GlEnv (ExceptT OkaneError IO)

-- | Run an Okane computation
runOkane :: Okane a -> IO (Either OkaneError a)
runOkane c = runExceptT $ evalStateT c initialEnv

instance MonadOkane Okane where
  addStep = return ()
  addOpp = return ()
  addMaxStack _ = return ()
  addClos = return ()