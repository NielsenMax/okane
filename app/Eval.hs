module Eval (runEval, runEvalWithState, runEvalStatements, showAccounts, initAccount, initialState, EvalState(..), EvalError, saveAccountsToDSLFile) where

import Lang
import MonadOkane
import qualified Global as G
import qualified Errors as E
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except (catchError)

-- | Evaluation state
data EvalState = EvalState
  { accounts :: Map String (Map String Int)
  } deriving (Show, Eq)

-- | Evaluation errors (reuse from Errors module)
type EvalError = E.OkaneError

-- | Initial state
initialState :: EvalState
initialState = EvalState Map.empty

-- | Helper functions for percentage calculations
calcPercentage :: Perc -> Int -> Int
calcPercentage (Perc p) amount = (amount * p) `div` 100
calcPercentage (Rat n d) amount = (amount * n) `div` d

-- | Validate percentage
validatePercentage :: Perc -> Either EvalError Perc
validatePercentage (Perc p) 
  | p < 0 || p > 100 = Left $ E.InvalidPercentage p
  | otherwise = Right $ Perc p
validatePercentage (Rat n d)
  | d == 0 = Left E.DivisionByZero
  | n < 0 || d < 0 = Left $ E.InvalidPercentage n
  | otherwise = Right $ Rat n d

-- | Get account balance for a specific coin
getBalance :: String -> String -> StateT EvalState (Except EvalError) Int
getBalance account coin = do
  state <- get
  let accountMap = Map.findWithDefault Map.empty account (accounts state)
  return $ Map.findWithDefault 0 coin accountMap

-- | Set account balance for a specific coin
setBalance :: String -> String -> Int -> StateT EvalState (Except EvalError) ()
setBalance account coin amount = do
  state <- get
  let accountMap = Map.findWithDefault Map.empty account (accounts state)
  let newAccountMap = if amount == 0 
                      then Map.delete coin accountMap
                      else Map.insert coin amount accountMap
  let newAccounts = Map.insert account newAccountMap (accounts state)
  put $ state { accounts = newAccounts }

-- | Add to account balance
addBalance :: String -> String -> Int -> StateT EvalState (Except EvalError) ()
addBalance account coin amount = do
  current <- getBalance account coin
  setBalance account coin (current + amount)

-- | Subtract from account balance (with validation)
subBalance :: String -> String -> Int -> StateT EvalState (Except EvalError) ()
subBalance account coin amount = do
  current <- getBalance account coin
  if current >= amount
    then setBalance account coin (current - amount)
    else throwError $ E.InsufficientFunds account coin amount current

-- | Evaluate a source to get the amount to transfer
evalSrc :: Src String -> String -> StateT EvalState (Except EvalError) Int
evalSrc (SAcc account) coin = getBalance account coin
evalSrc (SPerc perc srcs) coin = do
  case validatePercentage perc of
    Left err -> throwError err
    Right _ -> do
      totalAmount <- sum <$> mapM (`evalSrc` coin) srcs
      return $ calcPercentage perc totalAmount
evalSrc (SMax maxAmount srcs) coin = do
  availableAmount <- sum <$> mapM (`evalSrc` coin) srcs
  return $ min maxAmount availableAmount
evalSrc (SRem srcs) coin = do
  totalAmount <- sum <$> mapM (`evalSrc` coin) srcs
  return totalAmount

-- | Transfer from a source
transferFromSrc :: Src String -> String -> Int -> StateT EvalState (Except EvalError) ()
transferFromSrc (SAcc account) coin amount = subBalance account coin amount
transferFromSrc (SPerc perc srcs) coin amount = do
  case validatePercentage perc of
    Left err -> throwError err
    Right _ -> do
      let totalAmount = calcPercentage perc amount
      let perSrcAmount = if null srcs then 0 else totalAmount `div` length srcs
      mapM_ (\src -> transferFromSrc src coin perSrcAmount) srcs
transferFromSrc (SMax maxAmount srcs) coin amount = do
  let transferAmount = min maxAmount amount
  let perSrcAmount = if null srcs then 0 else transferAmount `div` length srcs
  mapM_ (\src -> transferFromSrc src coin perSrcAmount) srcs
transferFromSrc (SRem srcs) coin amount = do
  let perSrcAmount = if null srcs then 0 else amount `div` length srcs
  mapM_ (\src -> transferFromSrc src coin perSrcAmount) srcs

-- | Transfer to a destination
transferToDst :: Dst String -> String -> Int -> StateT EvalState (Except EvalError) ()
transferToDst (DAcc account) coin amount = addBalance account coin amount
transferToDst (DPerc perc dsts) coin amount = do
  case validatePercentage perc of
    Left err -> throwError err
    Right _ -> do
      let distributedAmount = calcPercentage perc amount
      let perDstAmount = if null dsts then 0 else distributedAmount `div` length dsts
      mapM_ (\dst -> transferToDst dst coin perDstAmount) dsts
transferToDst (DRem dsts) coin amount = do
  let perDstAmount = if null dsts then 0 else amount `div` length dsts
  mapM_ (\dst -> transferToDst dst coin perDstAmount) dsts

-- | Evaluate a statement
evalStm :: STerm -> StateT EvalState (Except EvalError) ()
evalStm (SSend srcs coin dsts) = do
  (coinName, amount) <- case coin of
        Coin name amt -> return (name, amt)
  -- Calculate total amount available from sources
  totalFromSrcs <- sum <$> mapM (`evalSrc` coinName) srcs
  -- Check if we have enough funds
  if totalFromSrcs < amount
    then throwError $ E.InsufficientFunds "sources" coinName amount totalFromSrcs
    else do
      -- Transfer from source accounts
      mapM_ (\src -> transferFromSrc src coinName amount) srcs
      -- Transfer to destination accounts
      mapM_ (\dst -> transferToDst dst coinName amount) dsts
evalStm (STrx stms) = do
  -- Save checkpoint before executing transaction
  checkpoint <- get
  -- Execute all statements with rollback on failure
  catchError (mapM_ evalStm stms) $ \err -> do
    -- Rollback to checkpoint on any error
    put checkpoint
    throwError err

evalStm (SAccount accountName coinName amount) = do
  -- Set account balance
  state <- get
  let accountMap = Map.findWithDefault Map.empty accountName (accounts state)
      newAccountMap = Map.insert coinName amount accountMap
      newAccounts = Map.insert accountName newAccountMap (accounts state)
  put $ state { accounts = newAccounts }

-- | Run evaluation with initial state
runEvalWithState :: STerm -> EvalState -> Either EvalError EvalState
runEvalWithState stm state = runExcept $ execStateT (evalStm stm) state

-- | Run multiple statements with initial state
runEvalStatements :: [STerm] -> EvalState -> Either EvalError EvalState
runEvalStatements stms state = runExcept $ execStateT (mapM_ evalStm stms) state

-- | Run evaluation with empty initial state
runEval :: STerm -> Either EvalError EvalState
runEval stm = runEvalWithState stm initialState

-- | Pretty print account balances
showAccounts :: Map String (Map String Int) -> String
showAccounts accs = 
  if Map.null accs
    then "No accounts"
    else unlines $ map showAccount $ Map.toList accs
  where
    showAccount (accountName, coins) =
      accountName ++ ":" ++ 
      if Map.null coins
        then " (empty)"
        else "\n" ++ unlines (map showCoin $ Map.toList coins)
    showCoin (coinName, amount) = "  " ++ coinName ++ ": " ++ show amount

-- | Helper function to create initial account with balance
initAccount :: String -> String -> Int -> EvalState -> EvalState
initAccount account coin amount state =
  let accountMap = Map.findWithDefault Map.empty account (accounts state)
      newAccountMap = Map.insert coin amount accountMap
      newAccounts = Map.insert account newAccountMap (accounts state)
  in state { accounts = newAccounts }

-- | Serialize accounts to DSL syntax format
serializeAccountsToDSL :: Map String (Map String Int) -> String
serializeAccountsToDSL accs = 
  if Map.null accs
    then ""
    else 
      let allStatements = concatMap serializeAccountToDSL $ Map.toList accs
      in unlines allStatements
  where
    serializeAccountToDSL (account, coins) = 
      map (\(coin, amount) -> "account " ++ account ++ " " ++ coin ++ " " ++ show amount) $ Map.toList coins


-- | Save accounts to a file in DSL format
saveAccountsToDSLFile :: String -> Map String (Map String Int) -> IO (Either String ())
saveAccountsToDSLFile filename accs = do
  let content = serializeAccountsToDSL accs
  writeFile filename content
  return $ Right ()
