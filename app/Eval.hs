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
evalSrc (SSingle account) coin = getBalance account coin
evalSrc (SMultiple connector) coin = evalSrcConnector connector coin

-- | Evaluate a source connector
evalSrcConnector :: SrcConnector String -> String -> StateT EvalState (Except EvalError) Int
evalSrcConnector (SConnPerc perc source) coin = do
  case validatePercentage perc of
    Left err -> throwError err
    Right _ -> do
      totalAmount <- evalSrc source coin
      return $ calcPercentage perc totalAmount
evalSrcConnector (SConnMax maxAmount source) coin = do
  availableAmount <- evalSrc source coin
  return $ min maxAmount availableAmount
evalSrcConnector (SConnRem source) coin = do
  totalAmount <- evalSrc source coin
  return totalAmount

-- | Transfer from a source
transferFromSrc :: Src String -> String -> Int -> StateT EvalState (Except EvalError) ()
transferFromSrc (SSingle account) coin amount = subBalance account coin amount
transferFromSrc (SMultiple connector) coin amount = transferFromSrcConnector connector coin amount

-- | Transfer from a source connector
transferFromSrcConnector :: SrcConnector String -> String -> Int -> StateT EvalState (Except EvalError) ()
transferFromSrcConnector (SConnPerc perc source) coin amount = do
  case validatePercentage perc of
    Left err -> throwError err
    Right _ -> do
      let totalAmount = calcPercentage perc amount
      transferFromSrc source coin totalAmount
transferFromSrcConnector (SConnMax maxAmount source) coin amount = do
  let transferAmount = min maxAmount amount
  transferFromSrc source coin transferAmount
transferFromSrcConnector (SConnRem source) coin amount = do
  transferFromSrc source coin amount

-- | Transfer to a destination
transferToDst :: Dst String -> String -> Int -> StateT EvalState (Except EvalError) ()
transferToDst (DSingle account) coin amount = addBalance account coin amount
transferToDst (DMultiple connector) coin amount = transferToDstConnector connector coin amount

-- | Transfer to a destination connector
transferToDstConnector :: DstConnector String -> String -> Int -> StateT EvalState (Except EvalError) ()
transferToDstConnector (DConnPerc perc destination) coin amount = do
  case validatePercentage perc of
    Left err -> throwError err
    Right _ -> do
      let distributedAmount = calcPercentage perc amount
      transferToDst destination coin distributedAmount
transferToDstConnector (DConnRem destination) coin amount = do
  transferToDst destination coin amount

-- | Calculate the maximum possible amount that can be withdrawn from a source
-- This uses the maximum values for all max expressions, but doesn't check account balances
calcMaxAmount :: Src String -> Int -> Either EvalError Int
calcMaxAmount (SSingle _) totalAmount = Right totalAmount  -- Single account can contribute up to total amount
calcMaxAmount (SMultiple connector) totalAmount = calcMaxAmountConnector connector totalAmount

-- | Calculate maximum amount for a source connector
calcMaxAmountConnector :: SrcConnector String -> Int -> Either EvalError Int
calcMaxAmountConnector (SConnPerc perc source) totalAmount = do
  case validatePercentage perc of
    Left err -> Left err  -- Throw error immediately for invalid percentage
    Right _ -> do
      maxFromSource <- calcMaxAmount source totalAmount
      return $ calcPercentage perc maxFromSource
calcMaxAmountConnector (SConnMax maxAmount source) totalAmount = do
  maxFromSource <- calcMaxAmount source totalAmount
  return $ min maxAmount maxFromSource  -- Return actual max amount, not 100%
calcMaxAmountConnector (SConnRem source) totalAmount = do
  maxFromSource <- calcMaxAmount source totalAmount
  return maxFromSource

-- | Calculate the minimum possible amount that can be withdrawn from a source
-- This uses 0 for all max expressions (they contribute nothing)
calcMinAmount :: Src String -> Int -> Either EvalError Int
calcMinAmount (SSingle _) totalAmount = Right 0  -- Single account can contribute 0
calcMinAmount (SMultiple connector) totalAmount = calcMinAmountConnector connector totalAmount

-- | Calculate minimum amount for a source connector
calcMinAmountConnector :: SrcConnector String -> Int -> Either EvalError Int
calcMinAmountConnector (SConnPerc perc source) totalAmount = do
  case validatePercentage perc of
    Left err -> Left err  -- Throw error immediately for invalid percentage
    Right _ -> do
      minFromSource <- calcMinAmount source totalAmount
      return $ calcPercentage perc minFromSource
calcMinAmountConnector (SConnMax _ source) totalAmount = do
  minFromSource <- calcMinAmount source totalAmount
  return 0  -- Max expressions contribute 0 to minimum
calcMinAmountConnector (SConnRem source) totalAmount = do
  minFromSource <- calcMinAmount source totalAmount
  return minFromSource

-- | Validate that the source percentage expressions are valid
-- This checks both that we don't exceed 100% (max amount >= total amount) 
-- and that we don't fall short of 100% (min amount <= total amount)
validateSourcePercentages :: Src String -> Int -> Either EvalError ()
validateSourcePercentages src totalAmount = do
  maxAmount <- calcMaxAmount src totalAmount
  minAmount <- calcMinAmount src totalAmount
  
  -- Check if we're trying to withdraw more than the maximum possible (over 100%)
  if maxAmount < totalAmount
    then Left $ E.InvalidPercentageSum (maxAmount * 100 `div` totalAmount) 100
    -- Check if we're trying to withdraw less than the minimum possible (under 100%)
    else if minAmount > totalAmount
    then Left $ E.InvalidPercentageSum (minAmount * 100 `div` totalAmount) 100
    else Right ()

-- | Calculate the maximum possible amount that can be distributed to a destination
-- This uses the maximum values for all percentage expressions
calcMaxDestinationAmount :: Dst String -> Int -> Either EvalError Int
calcMaxDestinationAmount (DSingle _) totalAmount = Right totalAmount  -- Single account can receive up to total amount
calcMaxDestinationAmount (DMultiple connector) totalAmount = calcMaxDestinationAmountConnector connector totalAmount

-- | Calculate maximum destination amount for a destination connector
calcMaxDestinationAmountConnector :: DstConnector String -> Int -> Either EvalError Int
calcMaxDestinationAmountConnector (DConnPerc perc destination) totalAmount = do
  case validatePercentage perc of
    Left err -> Left err  -- Throw error immediately for invalid percentage
    Right _ -> do
      maxToDestination <- calcMaxDestinationAmount destination totalAmount
      return $ calcPercentage perc maxToDestination
calcMaxDestinationAmountConnector (DConnRem destination) totalAmount = do
  maxToDestination <- calcMaxDestinationAmount destination totalAmount
  return maxToDestination

-- | Calculate the minimum possible amount that can be distributed to a destination
-- This uses 0 for all percentage expressions (they contribute nothing)
calcMinDestinationAmount :: Dst String -> Int -> Either EvalError Int
calcMinDestinationAmount (DSingle _) totalAmount = Right 0  -- Single account can receive 0
calcMinDestinationAmount (DMultiple connector) totalAmount = calcMinDestinationAmountConnector connector totalAmount

-- | Calculate minimum destination amount for a destination connector
calcMinDestinationAmountConnector :: DstConnector String -> Int -> Either EvalError Int
calcMinDestinationAmountConnector (DConnPerc perc destination) totalAmount = do
  case validatePercentage perc of
    Left err -> Left err  -- Throw error immediately for invalid percentage
    Right _ -> do
      minToDestination <- calcMinDestinationAmount destination totalAmount
      return $ calcPercentage perc minToDestination
calcMinDestinationAmountConnector (DConnRem destination) totalAmount = do
  minToDestination <- calcMinDestinationAmount destination totalAmount
  return minToDestination

-- | Validate that the destination percentage expressions are valid
-- This checks both that we don't exceed 100% (max amount >= total amount) 
-- and that we don't fall short of 100% (min amount <= total amount)
validateDestinationPercentages :: Dst String -> Int -> Either EvalError ()
validateDestinationPercentages dst totalAmount = do
  maxAmount <- calcMaxDestinationAmount dst totalAmount
  minAmount <- calcMinDestinationAmount dst totalAmount
  
  -- Check if we're trying to distribute more than the maximum possible (over 100%)
  if maxAmount < totalAmount
    then Left $ E.InvalidPercentageSum (maxAmount * 100 `div` totalAmount) 100
    -- Check if we're trying to distribute less than the minimum possible (under 100%)
    else if minAmount > totalAmount
    then Left $ E.InvalidPercentageSum (minAmount * 100 `div` totalAmount) 100
    else Right ()

-- | Evaluate a statement
evalStm :: STerm -> StateT EvalState (Except EvalError) ()
evalStm (SSend src coin dst) = do
  (coinName, amount) <- case coin of
        Coin name amt -> return (name, amt)
  -- Validate that the source percentage expressions are valid
  case validateSourcePercentages src amount of
    Left err -> throwError err
    Right _ -> do
      -- Validate that the destination percentage expressions are valid
      case validateDestinationPercentages dst amount of
        Left err -> throwError err
        Right _ -> do
          -- Calculate total amount available from source
          totalFromSrc <- evalSrc src coinName
          -- Check if we have enough funds
          if totalFromSrc < amount
            then throwError $ E.InsufficientFunds "sources" coinName amount totalFromSrc
            else do
              -- Transfer from source account
              transferFromSrc src coinName amount
              -- Transfer to destination account
              transferToDst dst coinName amount
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
