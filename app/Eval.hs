module Eval (eval, runEval, runEvalWithState, runEvalStatements, showAccounts, initialState, EvalState(..), EvalError, saveAccountsToDSLFile) where

import qualified Data.Map as Map
import Lang
import Control.Monad (foldM)
import Data.List (partition)


type EvalState = Map.Map String (Map.Map String Int)

newtype EvalError = EvalError String deriving (Show, Eq)

eval :: EvalState -> STerm -> Either EvalError EvalState
eval accounts (SAccount accountName coinName amount) = do
  let accountMap = Map.findWithDefault Map.empty accountName accounts
      newAccountMap = Map.insert coinName amount accountMap
      newAccounts = Map.insert accountName newAccountMap accounts
  return newAccounts
eval accounts (STrx stms) = do
  let result = foldM eval accounts stms
  case result of
    Left _ -> return accounts
    Right newAccounts -> return newAccounts
eval accounts (SSend sources coin destinations) = do
  result <- processSrc accounts sources coin
  case result of
    (newAccounts, remaining) | remaining > 0 -> Left $ EvalError "Not enough funds"
                             | remaining < 0 -> Left $ EvalError "Wrong source expression 1"
                             | remaining == 0 -> do
      result <- processDest destinations coin newAccounts
      case result of
       (newAccounts, remaining) | remaining == 0 -> return newAccounts
                                | otherwise -> Left $ EvalError "Wrong destination expression"


processSrc :: EvalState -> Src String -> Coin -> Either EvalError (EvalState, Int)
processSrc accounts (SSingle account) (Coin coinName amount) = do
  let accountMap = Map.findWithDefault Map.empty account accounts
      accountBalance = Map.findWithDefault 0 coinName accountMap
      availableAmount = min accountBalance amount
  return (Map.insert account (Map.insert coinName (accountBalance - availableAmount) accountMap) accounts, amount - availableAmount)
processSrc accounts (SMultiple connectors) c@(Coin coinName amount) = let
  (cs, maxs) = partition f connectors
  sorted = uncurry (++) $ partition g cs
  in foldM (processSrcConnector c) (accounts, amount) $ sorted ++ maxs
  where f (SConnMax _ _) = False
        f _ = True
        g (SConnPerc _ _) = False
        g _ = True

processSrcConnector :: Coin -> (EvalState, Int) -> SrcConnector String -> Either EvalError (EvalState, Int)
processSrcConnector (Coin coinName _) (accounts, remaining) (SConnRem source) =  processSrc accounts source (Coin coinName remaining)
processSrcConnector (Coin coinName _) (accounts, remaining) (SConnMax maxAmount source) = do
  let amount = min maxAmount remaining
  (newAccounts, newRemaining) <- processSrc accounts source (Coin coinName amount)
  return (newAccounts, remaining - amount + newRemaining)
processSrcConnector (Coin coinName amount) (accounts, remaining) (SConnPerc perc source) = do
  parsedAmount <- calcPercentage perc amount
  result <- if parsedAmount > remaining then Left $ EvalError "Wrong source expression 2" else processSrc accounts source (Coin coinName parsedAmount)
  case result of
    (newAccounts, remaining2) | remaining2 == 0 -> return (newAccounts, remaining - parsedAmount + remaining2)
                              | remaining2 > 0 -> Left $ EvalError "Not enough funds"
                              | otherwise -> Left $ EvalError "Wrong source expression 3"
  where calcPercentage (Perc p) amount | p < 0 || p > 100 = Left $ EvalError "Invalid percentage"
                                       | otherwise = Right $ (amount * p) `div` 100
        calcPercentage (Rat n d) amount | d == 0 = Left $ EvalError "Division by zero"
                                        | n > d = Left $ EvalError "Invalid percentage"
                                        | otherwise = Right $ (amount * n) `div` d

processDest :: Dst String -> Coin -> EvalState -> Either EvalError (EvalState, Int)
processDest (DSingle account) (Coin coinName amount) accounts = do
  let accountMap = Map.findWithDefault Map.empty account accounts
      accountBalance = Map.findWithDefault 0 coinName accountMap
  return (Map.insert account (Map.insert coinName (accountBalance + amount) accountMap) accounts, 0)
processDest (DMultiple connectors) c@(Coin coinName amount) accounts = let
  sorted = uncurry (++) $ partition f connectors
  in foldM (processDestConnector c) (accounts, amount) sorted
  where f (DConnRem _) = False
        f _ = True

processDestConnector :: Coin -> (EvalState, Int) -> DstConnector String -> Either EvalError (EvalState, Int)
processDestConnector (Coin coinName _) (accounts, remaining) (DConnRem destination) = processDest destination (Coin coinName remaining) accounts
processDestConnector (Coin coinName amount) (accounts, remaining) (DConnPerc perc destination) = do
  parsedAmount <- calcPercentage perc amount
  result <- if parsedAmount > remaining then Left $ EvalError "Wrong destination expression" else processDest destination (Coin coinName parsedAmount) accounts
  case result of
    (newAccounts, remaining2) | remaining2 == 0 -> return (newAccounts, remaining - parsedAmount + remaining2)
                             | otherwise -> Left $ EvalError "Wrong destination expression"
  where calcPercentage (Perc p) amount | p < 0 || p > 100 = Left $ EvalError "Invalid percentage"
                                       | otherwise = Right $ (amount * p) `div` 100
        calcPercentage (Rat n d) amount | d == 0 = Left $ EvalError "Division by zero"
                                        | n > d = Left $ EvalError "Invalid percentage"
                                        | otherwise = Right $ (amount * n) `div` d

-- | Initial state
initialState :: EvalState
initialState = Map.empty

-- | Run evaluation with initial state
runEvalWithState :: STerm -> EvalState -> Either EvalError EvalState
runEvalWithState stm state = eval state stm

-- | Run multiple statements with initial state
runEvalStatements :: [STerm] -> EvalState -> Either EvalError EvalState
runEvalStatements [] state = return state
runEvalStatements (stm:stms) state = let
  result = eval state stm
  in case result of
    Left _ -> runEvalStatements stms state
    Right newState -> runEvalStatements stms newState

-- | Run evaluation with empty initial state
runEval :: STerm -> Either EvalError EvalState
runEval stm = runEvalWithState stm initialState

-- | Pretty print account balances
showAccounts :: EvalState -> String
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

-- | Serialize accounts to DSL syntax format
serializeAccountsToDSL :: EvalState -> String
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
saveAccountsToDSLFile :: String -> EvalState -> IO (Either String ())
saveAccountsToDSLFile filename accs = do
  let content = serializeAccountsToDSL accs
  writeFile filename content
  return $ Right ()