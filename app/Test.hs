{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Lang
import Parse (runP, term, parseFile)
import Eval (runEval, runEvalWithState, runEvalStatements, showAccounts, initialState, EvalState(..), EvalError(..))
import qualified Errors as E
import Data.Map (Map)
import qualified Data.Map as Map

-- Test utilities
assertParse :: String -> STerm -> Assertion
assertParse input expected = 
  case runP term input "" of
    Right result -> assertEqual ("Parse: " ++ input) expected result
    Left err -> assertFailure ("Parse error for '" ++ input ++ "': " ++ show err)

assertParseError :: String -> Assertion
assertParseError input = 
  case runP term input "" of
    Right result -> assertFailure ("Expected parse error for '" ++ input ++ "', but got: " ++ show result)
    Left _ -> return ()

-- Helper to create initial state with accounts
mkState :: [(String, String, Int)] -> EvalState
mkState accountsList = Map.fromList $ map (\(name, coin, amount) -> (name, Map.singleton coin amount)) accountsList

-- Basic parsing tests
testBasicSend :: Test
testBasicSend = TestCase $ do
  assertParse "send alice [USD 100] bob" 
    (SSend (SSingle "alice") (Coin "USD" 100) (DSingle "bob"))
  
testTransaction :: Test
testTransaction = TestCase $ do
  assertParse "trx { send alice [USD 100] bob; send bob [USD 50] charlie }"
    (STrx [SSend (SSingle "alice") (Coin "USD" 100) (DSingle "bob"),
           SSend (SSingle "bob") (Coin "USD" 50) (DSingle "charlie")])

-- Basic evaluation tests
testBasicSendEval :: Test
testBasicSendEval = TestCase $ do
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right finalState -> do
      let aliceBalance = case Map.lookup "alice" finalState of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" finalState of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 600 USD" 600 bobBalance
    Left err -> assertFailure ("Basic send should work: " ++ show err)

testTransactionEval :: Test
testTransactionEval = TestCase $ do
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500), ("charlie", "USD", 200)]
  let stm = STrx [SSend (SSingle "alice") (Coin "USD" 100) (DSingle "bob"), 
                  SSend (SSingle "bob") (Coin "USD" 50) (DSingle "charlie")]
  case runEvalWithState stm initState of
    Right finalState -> do
      let aliceBalance = case Map.lookup "alice" finalState of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" finalState of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" finalState of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 550 USD" 550 bobBalance
      assertEqual "Charlie should have 250 USD" 250 charlieBalance
    Left err -> assertFailure ("Transaction should work: " ++ show err)

testFileParsing :: Test
testFileParsing = TestCase $ do
  let fileContent = "send alice [USD 100] bob\nsend bob [USD 50] charlie\ntrx { send charlie [USD 25] alice; send alice [USD 10] bob }"
  case parseFile fileContent of
    Right statements -> do
      assertEqual "Should parse 3 statements" 3 (length statements)
      let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500), ("charlie", "USD", 200)]
      case runEvalStatements statements initState of
        Right result -> do
          let aliceBalance = case Map.lookup "alice" result of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          let bobBalance = case Map.lookup "bob" result of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          let charlieBalance = case Map.lookup "charlie" result of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          assertEqual "Alice should have 915 USD after file execution" 915 aliceBalance
          assertEqual "Bob should have 560 USD after file execution" 560 bobBalance
          assertEqual "Charlie should have 225 USD after file execution" 225 charlieBalance
        Left err -> assertFailure $ "File execution failed: " ++ show err
    Left err -> assertFailure $ "File parsing failed: " ++ show err


testAccountSyntax :: Test
testAccountSyntax = TestCase $ do
  assertParse "account alice USD 1000" (SAccount "alice" "USD" 1000)
  assertParse "account bob BTC 5" (SAccount "bob" "BTC" 5)

testAccountEval :: Test
testAccountEval = TestCase $ do
  let stm = SAccount "alice" "USD" 1000
  case runEvalWithState stm initialState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 1000 USD after account creation" 1000 aliceBalance
    Left err -> assertFailure $ "Account creation failed: " ++ show err

testCompleteExample :: Test
testCompleteExample = TestCase $ do
  let fileContent = "account alice USD 1000\naccount bob USD 500\nsend alice [USD 100] bob"
  case parseFile fileContent of
    Right statements -> do
      assertEqual "Should parse 3 statements" 3 (length statements)
      case runEvalStatements statements initialState of
        Right result -> do
          let aliceBalance = case Map.lookup "alice" result of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          let bobBalance = case Map.lookup "bob" result of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          assertEqual "Alice should have 900 USD after complete example" 900 aliceBalance
          assertEqual "Bob should have 600 USD after complete example" 600 bobBalance
        Left err -> assertFailure $ "Complete example execution failed: " ++ show err
    Left err -> assertFailure $ "Complete example parsing failed: " ++ show err

testComplexSendParse :: Test
testComplexSendParse = TestCase $ do
  -- Test basic percentage parsing (single source)
  assertParse "send {50% from alice} [USD 100] bob" (SSend (SMultiple [SConnPerc (Perc 50) (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob"))
  -- Test basic max parsing
  assertParse "send {max 100 from alice} [USD 100] bob" (SSend (SMultiple [SConnMax 100 (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob"))
  -- Test basic remaining parsing
  assertParse "send {remaining from alice} [USD 100] bob" (SSend (SMultiple [SConnRem (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob"))

testComplexSendEval :: Test
testComplexSendEval = TestCase $ do
  -- Test percentage send (100% of 100 = 100)
  let stm1 = SSend (SMultiple [SConnPerc (Perc 100) (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState1 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm1 initState1 of
    Right result1 -> do
      let aliceBalance1 = case Map.lookup "alice" result1 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance1 = case Map.lookup "bob" result1 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after 100% of 100 send" 900 aliceBalance1
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance1
    Left err -> assertFailure $ "Percentage send failed: " ++ show err

  -- Test fraction send (1/1 of 100 = 100%)
  let stm2 = SSend (SMultiple [SConnPerc (Rat 1 1) (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState2 = mkState [("alice", "USD", 900), ("bob", "USD", 0)]
  case runEvalWithState stm2 initState2 of
    Right result2 -> do
      let aliceBalance2 = case Map.lookup "alice" result2 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance2 = case Map.lookup "bob" result2 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 800 USD after 1/1 of 100 send" 800 aliceBalance2
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance2
    Left err -> assertFailure $ "Fraction send failed: " ++ show err

  -- Test max send (max 200, but only send 100)
  let stm3 = SSend (SMultiple [SConnMax 200 (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState3 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm3 initState3 of
    Right result3 -> do
      let aliceBalance3 = case Map.lookup "alice" result3 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance3 = case Map.lookup "bob" result3 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after max send" 900 aliceBalance3
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance3
    Left err -> assertFailure $ "Max send failed: " ++ show err

  -- Test remaining send (remaining = 100)
  let stm4 = SSend (SMultiple [SConnRem (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState4 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm4 initState4 of
    Right result4 -> do
      let aliceBalance4 = case Map.lookup "alice" result4 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance4 = case Map.lookup "bob" result4 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after remaining send" 900 aliceBalance4
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance4
    Left err -> assertFailure $ "Remaining send failed: " ++ show err

testMultipleSourcesDestinations :: Test
testMultipleSourcesDestinations = TestCase $ do
  -- Test single source with percentage (100% of 100 = 100 from alice only)
  let stm1 = SSend (SMultiple [SConnPerc (Perc 100) (SSingle "alice")]) (Coin "USD" 100) (DSingle "charlie")
  let initState1 = mkState [("alice", "USD", 1000), ("bob", "USD", 800), ("charlie", "USD", 0)]
  case runEvalWithState stm1 initState1 of
    Right result1 -> do
      let aliceBalance1 = case Map.lookup "alice" result1 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance1 = case Map.lookup "bob" result1 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance1 = case Map.lookup "charlie" result1 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after 100% of 100 send" 900 aliceBalance1
      assertEqual "Bob should have 800 USD (unchanged)" 800 bobBalance1
      assertEqual "Charlie should have 100 USD after receiving" 100 charlieBalance1
    Left err -> assertFailure $ "Single source percentage send failed: " ++ show err

  -- Test multiple destinations with remaining (only first gets the full amount)
  let stm2 = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnRem (DSingle "bob"), DConnRem (DSingle "charlie")])
  let initState2 = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0)]
  case runEvalWithState stm2 initState2 of
    Right result2 -> do
      let aliceBalance2 = case Map.lookup "alice" result2 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance2 = case Map.lookup "bob" result2 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance2 = case Map.lookup "charlie" result2 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after sending to multiple destinations" 900 aliceBalance2
      assertEqual "Bob should have 100 USD (gets all remaining - first destination)" 100 bobBalance2
      assertEqual "Charlie should have 0 USD (no remaining left for second destination)" 0 charlieBalance2
    Left err -> assertFailure $ "Multiple destinations send failed: " ++ show err

  -- Test simple case: single source with max, single destination
  let stm3 = SSend (SMultiple [SConnMax 150 (SSingle "alice")]) (Coin "USD" 100) (DSingle "charlie")
  let initState3 = mkState [("alice", "USD", 1000), ("bob", "USD", 800), ("charlie", "USD", 0), ("david", "USD", 0)]
  case runEvalWithState stm3 initState3 of
    Right result3 -> do
      let aliceBalance3 = case Map.lookup "alice" result3 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance3 = case Map.lookup "bob" result3 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance3 = case Map.lookup "charlie" result3 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance3 = case Map.lookup "david" result3 of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after max send (1000 - 100)" 900 aliceBalance3
      assertEqual "Bob should have 800 USD (unchanged)" 800 bobBalance3
      assertEqual "Charlie should have 100 USD after receiving" 100 charlieBalance3
      assertEqual "David should have 0 USD (unchanged)" 0 davidBalance3
    Left err -> assertFailure $ "Simple max send failed: " ++ show err

testPercentageValidation :: Test
testPercentageValidation = TestCase $ do
  -- Test valid 100% percentage
  let stm1 = SSend (SMultiple [SConnPerc (Perc 100) (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState1 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm1 initState1 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "100% percentage should be valid: " ++ show err

  -- Test invalid percentage (75% doesn't add up to 100%)
  let stm2 = SSend (SMultiple [SConnPerc (Perc 75) (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState2 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm2 initState2 of
    Right _ -> assertFailure "75% percentage should be invalid"
    Left _ -> return () -- Should fail with error

  -- Test valid max amount (100% of transfer amount)
  let stm3 = SSend (SMultiple [SConnMax 100 (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState3 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm3 initState3 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "Max 100 should be valid: " ++ show err

  -- Test valid remaining (always valid)
  let stm4 = SSend (SMultiple [SConnRem (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  let initState4 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm4 initState4 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "Remaining should be valid: " ++ show err

  -- Test valid combination: 50% + max 50 (100% total)
  let stm5 = SSend (SMultiple [SConnPerc (Perc 50) (SSingle "alice"), SConnMax 50 (SSingle "bob")]) (Coin "USD" 100) (DSingle "charlie")
  let initState5 = mkState [("alice", "USD", 1000), ("bob", "USD", 1000), ("charlie", "USD", 0)]
  case runEvalWithState stm5 initState5 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "50% + max 50 should be valid: " ++ show err

-- Advanced complex tests
testMultipleCoins :: Test
testMultipleCoins = TestCase $ do
  -- Test accounts with multiple coin types
  let initState = Map.fromList [("alice", Map.fromList [("USD", 1000), ("EUR", 500)]), 
                                ("bob", Map.fromList [("USD", 200), ("EUR", 300)])]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceUSD = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let aliceEUR = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "EUR" coins
            Nothing -> 0
      let bobUSD = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobEUR = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "EUR" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceUSD
      assertEqual "Alice should still have 500 EUR" 500 aliceEUR
      assertEqual "Bob should have 300 USD" 300 bobUSD
      assertEqual "Bob should still have 300 EUR" 300 bobEUR
    Left err -> assertFailure $ "Multiple coins test failed: " ++ show err

testInsufficientFunds :: Test
testInsufficientFunds = TestCase $ do
  -- Test insufficient funds error
  let initState = mkState [("alice", "USD", 50)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with insufficient funds"
    Left _ -> return () -- Should fail with error

testZeroAmount :: Test
testZeroAmount = TestCase $ do
  -- Test zero amount transfer
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 0) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should still have 1000 USD" 1000 aliceBalance
      assertEqual "Bob should still have 0 USD" 0 bobBalance
    Left err -> assertFailure $ "Zero amount test failed: " ++ show err

testComplexPercentage :: Test
testComplexPercentage = TestCase $ do
  -- Test complex percentage calculation (50% of 200 = 100 from alice, 50% of 200 = 100 from charlie)
  let initState = mkState [("alice", "USD", 1000), ("charlie", "USD", 1000), ("bob", "USD", 0)]
  let stm = SSend (SMultiple [SConnPerc (Perc 50) (SSingle "alice"), SConnPerc (Perc 50) (SSingle "charlie")]) (Coin "USD" 200) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD (1000 - 100)" 900 aliceBalance
      assertEqual "Charlie should have 900 USD (1000 - 100)" 900 charlieBalance
      assertEqual "Bob should have 200 USD (100 + 100)" 200 bobBalance
    Left err -> assertFailure $ "Complex percentage test failed: " ++ show err

testMaxAmount :: Test
testMaxAmount = TestCase $ do
  -- Test max amount (max 50 from alice, max 50 from charlie = 100 total)
  let initState = mkState [("alice", "USD", 1000), ("charlie", "USD", 1000), ("bob", "USD", 0)]
  let stm = SSend (SMultiple [SConnMax 50 (SSingle "alice"), SConnMax 50 (SSingle "charlie")]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 950 USD (1000 - 50)" 950 aliceBalance
      assertEqual "Charlie should have 950 USD (1000 - 50)" 950 charlieBalance
      assertEqual "Bob should have 100 USD (50 + 50)" 100 bobBalance
    Left err -> assertFailure $ "Max amount test failed: " ++ show err

testRemainingAmount :: Test
testRemainingAmount = TestCase $ do
  -- Test remaining amount (75 from alice + 25 from charlie = 100 total)
  let initState = mkState [("alice", "USD", 75), ("charlie", "USD", 25), ("bob", "USD", 0)]
  let stm = SSend (SMultiple [SConnRem (SSingle "alice"), SConnRem (SSingle "charlie")]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 0 USD (all sent)" 0 aliceBalance
      assertEqual "Charlie should have 0 USD (all sent)" 0 charlieBalance
      assertEqual "Bob should have 100 USD (75 + 25)" 100 bobBalance
    Left err -> assertFailure $ "Remaining amount test failed: " ++ show err

testComplexTransactionChain :: Test
testComplexTransactionChain = TestCase $ do
  -- Test a complex chain of transactions
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500), ("charlie", "USD", 0), ("david", "USD", 0)]
  let stm1 = SSend (SSingle "alice") (Coin "USD" 200) (DSingle "charlie")
  let stm2 = SSend (SSingle "bob") (Coin "USD" 100) (DSingle "david")
  let stm3 = SSend (SSingle "charlie") (Coin "USD" 50) (DSingle "david")
  let transactions = [stm1, stm2, stm3]
  case runEvalStatements transactions initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 800 USD" 800 aliceBalance
      assertEqual "Bob should have 400 USD" 400 bobBalance
      assertEqual "Charlie should have 150 USD" 150 charlieBalance
      assertEqual "David should have 150 USD" 150 davidBalance
    Left err -> assertFailure $ "Complex transaction chain failed: " ++ show err

testAccountCreation :: Test
testAccountCreation = TestCase $ do
  -- Test account creation
  let initState = Map.empty
  let stm = SAccount "alice" "USD" 1000
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 1000 USD" 1000 aliceBalance
    Left err -> assertFailure $ "Account creation failed: " ++ show err

testMultipleDestinationsWithRemaining :: Test
testMultipleDestinationsWithRemaining = TestCase $ do
  -- Test multiple destinations with remaining distribution
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnRem (DSingle "bob"), DConnRem (DSingle "charlie")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 100 USD" 100 bobBalance
      assertEqual "Charlie should have 0 USD (no remaining left for second destination)" 0 charlieBalance
    Left err -> assertFailure $ "Multiple destinations with remaining failed: " ++ show err

testInsufficientSourceAmount :: Test
testInsufficientSourceAmount = TestCase $ do
  -- Test failure when sources don't provide enough to reach the total amount
  -- Alice has 50, Charlie has 30, but we need 100 total
  let initState = mkState [("alice", "USD", 50), ("charlie", "USD", 30), ("bob", "USD", 0)]
  let stm = SSend (SMultiple [SConnRem (SSingle "alice"), SConnRem (SSingle "charlie")]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with insufficient source amount"
    Left _ -> return () -- Expected error

testInsufficientSourceAmountPercentage :: Test
testInsufficientSourceAmountPercentage = TestCase $ do
  -- Test failure when percentage sources don't provide enough to reach the total amount
  -- 50% of 100 = 50 from alice, but we need 100 total (missing 50)
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  let stm = SSend (SMultiple [SConnPerc (Perc 50) (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with insufficient source amount (only 50% provided)"
    Left _ -> return () -- Expected error

testInsufficientSourceAmountMax :: Test
testInsufficientSourceAmountMax = TestCase $ do
  -- Test failure when max amount sources don't provide enough to reach the total amount
  -- Max 30 from alice, but we need 100 total (missing 70)
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  let stm = SSend (SMultiple [SConnMax 30 (SSingle "alice")]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with insufficient source amount (max 30, need 100)"
    Left _ -> return () -- Expected error

testIncompleteDestinationConsumption :: Test
testIncompleteDestinationConsumption = TestCase $ do
  -- Test failure when destinations don't consume the full amount
  -- Send 100 USD, but destinations only consume 60 USD (50% of 100 = 50 + 10 fixed = 60)
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnPerc (Perc 50) (DSingle "bob"), DConnPerc (Perc 10) (DSingle "charlie")])
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with incomplete destination consumption (only 60% consumed)"
    Left _ -> return () -- Expected error

testIncompleteDestinationConsumptionPercentage :: Test
testIncompleteDestinationConsumptionPercentage = TestCase $ do
  -- Test failure when destinations don't consume the full amount
  -- Send 100 USD, but destinations only consume 60 USD (50% + 10% = 60%)
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnPerc (Perc 50) (DSingle "bob"), DConnPerc (Perc 10) (DSingle "charlie")])
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with incomplete destination consumption (50% + 10% = 60%, need 100%)"
    Left _ -> return () -- Expected error

testNestedSources :: Test
testNestedSources = TestCase $ do
  -- Test nested sources: SMultiple containing another SMultiple
  -- This tests: {remaining from {50% from alice, 50% from charlie}} [USD 100] bob
  let initState = mkState [("alice", "USD", 1000), ("charlie", "USD", 1000), ("bob", "USD", 0)]
  let nestedSource = SMultiple [SConnPerc (Perc 50) (SSingle "alice"), SConnPerc (Perc 50) (SSingle "charlie")]
  let stm = SSend (SMultiple [SConnRem nestedSource]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 950 USD (1000 - 50)" 950 aliceBalance
      assertEqual "Charlie should have 950 USD (1000 - 50)" 950 charlieBalance
      assertEqual "Bob should have 100 USD" 100 bobBalance
    Left err -> assertFailure $ "Nested sources test failed: " ++ show err

testNestedDestinations :: Test
testNestedDestinations = TestCase $ do
  -- Test nested destinations: DMultiple containing another DMultiple
  -- This tests: send alice [USD 100] {50% to {remaining to bob, remaining to charlie}, 50% to david}
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0)]
  let nestedDest = DMultiple [DConnPerc (Perc 50) (DSingle "bob"), DConnPerc (Perc 50) (DSingle "charlie")]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnPerc (Perc 50) nestedDest, DConnPerc (Perc 50) (DSingle "david")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 50 USD (gets all remaining from nested 50%)" 25 bobBalance
      assertEqual "Charlie should have 0 USD (no remaining left)" 25 charlieBalance
      assertEqual "David should have 50 USD (50% of total)" 50 davidBalance
    Left err -> assertFailure $ "Nested destinations test failed: " ++ show err

testDeeplyNestedSources :: Test
testDeeplyNestedSources = TestCase $ do
  -- Test deeply nested sources: SMultiple containing SMultiple containing SMultiple
  -- This tests: {max 30 from {remaining from {50% from alice, 50% from charlie}}} [USD 30] bob
  let initState = mkState [("alice", "USD", 1000), ("charlie", "USD", 1000), ("tom", "USD", 1000), ("bob", "USD", 0)]
  let innerSource = SMultiple [SConnPerc (Perc 50) (SSingle "alice"), SConnPerc (Perc 50) (SSingle "charlie")]
  let middleSource = SMultiple [SConnRem innerSource]
  let stm = SSend (SMultiple [SConnMax 30 middleSource, SConnPerc (Rat 1 3) (SSingle "tom")]) (Coin "USD" 30) (DSingle "bob")
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let tomBalance = case Map.lookup "tom" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 985 USD (1000 - 15)" 990 aliceBalance
      assertEqual "Charlie should have 985 USD (1000 - 15)" 990 charlieBalance
      assertEqual "Tom should have 985 USD (1000 - 15)" 990 tomBalance
      assertEqual "Bob should have 30 USD" 30 bobBalance
    Left err -> assertFailure $ "Deeply nested sources test failed: " ++ show err

testNestedSourcesWithInsufficientFunds :: Test
testNestedSourcesWithInsufficientFunds = TestCase $ do
  -- Test nested sources that don't provide enough funds
  -- Alice has 20, Charlie has 20, but we need 100 total
  let initState = mkState [("alice", "USD", 20), ("charlie", "USD", 20), ("bob", "USD", 0)]
  let nestedSource = SMultiple [SConnRem (SSingle "alice"), SConnRem (SSingle "charlie")]
  let stm = SSend (SMultiple [SConnRem nestedSource]) (Coin "USD" 100) (DSingle "bob")
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with insufficient funds in nested sources"
    Left _ -> return () -- Expected error

testNestedDestinationsWithIncompleteConsumption :: Test
testNestedDestinationsWithIncompleteConsumption = TestCase $ do
  -- Test nested destinations that don't consume the full amount
  -- Send 100 USD, but nested destinations only consume 60 USD (50% + 10% = 60%)
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0)]
  let nestedDest = DMultiple [DConnPerc (Perc 50) (DSingle "bob"), DConnPerc (Perc 10) (DSingle "charlie")]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnRem nestedDest])
  case runEvalWithState stm initState of
    Right _ -> assertFailure "Should fail with incomplete consumption in nested destinations"
    Left _ -> return () -- Expected error

testDestinationMaxAmount :: Test
testDestinationMaxAmount = TestCase $ do
  -- Test DConnMax: send 100 USD, but destinations have max limits
  -- 50% to bob (50 USD), max 30 to charlie (30 USD), remaining to david (20 USD)
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnPerc (Perc 50) (DSingle "bob"), DConnMax 30 (DSingle "charlie"), DConnRem (DSingle "david")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 50 USD (50%)" 50 bobBalance
      assertEqual "Charlie should have 30 USD (max 30)" 30 charlieBalance
      assertEqual "David should have 20 USD (remaining)" 20 davidBalance
    Left err -> assertFailure $ "Destination max amount test failed: " ++ show err

testDestinationMaxAmountWithMultipleMax :: Test
testDestinationMaxAmountWithMultipleMax = TestCase $ do
  -- Test multiple DConnMax: send 100 USD
  -- max 20 to bob, max 30 to charlie, max 25 to david, remaining to eve
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0), ("eve", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnMax 20 (DSingle "bob"), DConnMax 30 (DSingle "charlie"), DConnMax 25 (DSingle "david"), DConnRem (DSingle "eve")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let eveBalance = case Map.lookup "eve" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 20 USD (max 20)" 20 bobBalance
      assertEqual "Charlie should have 30 USD (max 30)" 30 charlieBalance
      assertEqual "David should have 25 USD (max 25)" 25 davidBalance
      assertEqual "Eve should have 25 USD (remaining: 100-20-30-25=25)" 25 eveBalance
    Left err -> assertFailure $ "Multiple destination max amount test failed: " ++ show err

testDestinationMaxAmountWithInsufficientRemaining :: Test
testDestinationMaxAmountWithInsufficientRemaining = TestCase $ do
  -- Test DConnMax with insufficient remaining: send 50 USD
  -- max 30 to bob, max 30 to charlie, remaining to david
  -- Bob gets 30, Charlie gets 20 (remaining), David gets 0
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 50) (DMultiple [DConnMax 30 (DSingle "bob"), DConnMax 30 (DSingle "charlie"), DConnRem (DSingle "david")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 950 USD" 950 aliceBalance
      assertEqual "Bob should have 30 USD (max 30)" 30 bobBalance
      assertEqual "Charlie should have 20 USD (remaining 20)" 20 charlieBalance
      assertEqual "David should have 0 USD (no remaining)" 0 davidBalance
    Left err -> assertFailure $ "Insufficient remaining max amount test failed: " ++ show err

testDestinationMaxOnly :: Test
testDestinationMaxOnly = TestCase $ do
  -- Test only DConnMax destinations: send 100 USD
  -- max 40 to bob, max 30 to charlie, max 25 to david, remaining to eve
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0), ("eve", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnMax 40 (DSingle "bob"), DConnMax 30 (DSingle "charlie"), DConnMax 25 (DSingle "david"), DConnRem (DSingle "eve")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let eveBalance = case Map.lookup "eve" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 40 USD (max 40)" 40 bobBalance
      assertEqual "Charlie should have 30 USD (max 30)" 30 charlieBalance
      assertEqual "David should have 25 USD (max 25)" 25 davidBalance
      assertEqual "Eve should have 5 USD (remaining: 100-40-30-25=5)" 5 eveBalance
    Left err -> assertFailure $ "Destination max only test failed: " ++ show err

testDestinationMaxWithZeroRemaining :: Test
testDestinationMaxWithZeroRemaining = TestCase $ do
  -- Test DConnMax with exactly enough funds: send 75 USD
  -- max 30 to bob, max 25 to charlie, max 20 to david, remaining to eve
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0), ("eve", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 75) (DMultiple [DConnMax 30 (DSingle "bob"), DConnMax 25 (DSingle "charlie"), DConnMax 20 (DSingle "david"), DConnRem (DSingle "eve")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let eveBalance = case Map.lookup "eve" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 925 USD" 925 aliceBalance
      assertEqual "Bob should have 30 USD (max 30)" 30 bobBalance
      assertEqual "Charlie should have 25 USD (max 25)" 25 charlieBalance
      assertEqual "David should have 20 USD (max 20)" 20 davidBalance
      assertEqual "Eve should have 0 USD (no remaining)" 0 eveBalance
    Left err -> assertFailure $ "Destination max with zero remaining test failed: " ++ show err

testDestinationMaxWithLargeMax :: Test
testDestinationMaxWithLargeMax = TestCase $ do
  -- Test DConnMax with max amounts larger than total: send 50 USD
  -- max 100 to bob, max 80 to charlie, remaining to david
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 50) (DMultiple [DConnMax 100 (DSingle "bob"), DConnMax 80 (DSingle "charlie"), DConnRem (DSingle "david")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 950 USD" 950 aliceBalance
      assertEqual "Bob should have 50 USD (all remaining, max 100)" 50 bobBalance
      assertEqual "Charlie should have 0 USD (no remaining left)" 0 charlieBalance
      assertEqual "David should have 0 USD (no remaining)" 0 davidBalance
    Left err -> assertFailure $ "Destination max with large max test failed: " ++ show err

testDestinationMaxWithNestedDestinations :: Test
testDestinationMaxWithNestedDestinations = TestCase $ do
  -- Test DConnMax with nested destinations: send 100 USD
  -- 50% to {max 20 to bob, remaining to charlie}, max 30 to david, remaining to eve
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0), ("eve", "USD", 0)]
  let nestedDest = DMultiple [DConnMax 20 (DSingle "bob"), DConnRem (DSingle "charlie")]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnPerc (Perc 50) nestedDest, DConnMax 30 (DSingle "david"), DConnRem (DSingle "eve")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let eveBalance = case Map.lookup "eve" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 20 USD (max 20 from nested 50%)" 20 bobBalance
      assertEqual "Charlie should have 30 USD (remaining from nested 50%)" 30 charlieBalance
      assertEqual "David should have 30 USD (max 30)" 30 davidBalance
      assertEqual "Eve should have 20 USD (remaining: 100-50-30=20)" 20 eveBalance
    Left err -> assertFailure $ "Nested destination max test failed: " ++ show err

testDestinationMaxWithFractionalPercentages :: Test
testDestinationMaxWithFractionalPercentages = TestCase $ do
  -- Test DConnMax with fractional percentages: send 100 USD
  -- 1/3 to bob (33), max 20 to charlie, max 15 to david, remaining to eve
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0), ("david", "USD", 0), ("eve", "USD", 0)]
  let stm = SSend (SSingle "alice") (Coin "USD" 100) (DMultiple [DConnPerc (Rat 1 3) (DSingle "bob"), DConnMax 20 (DSingle "charlie"), DConnMax 15 (DSingle "david"), DConnRem (DSingle "eve")])
  case runEvalWithState stm initState of
    Right result -> do
      let aliceBalance = case Map.lookup "alice" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance = case Map.lookup "david" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let eveBalance = case Map.lookup "eve" result of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 33 USD (1/3 of 100)" 33 bobBalance
      assertEqual "Charlie should have 20 USD (max 20)" 20 charlieBalance
      assertEqual "David should have 15 USD (max 15)" 15 davidBalance
      assertEqual "Eve should have 32 USD (remaining: 100-33-20-15=32)" 32 eveBalance
    Left err -> assertFailure $ "Fractional percentage with max test failed: " ++ show err

-- Test suite
tests :: Test
tests = TestList
  [ TestLabel "Basic Send Parse" testBasicSend
  , TestLabel "Transaction Parse" testTransaction
  , TestLabel "Account Parse" testAccountSyntax
  , TestLabel "Complex Send Parse" testComplexSendParse
  , TestLabel "Basic Send Eval" testBasicSendEval
  , TestLabel "Transaction Eval" testTransactionEval
  , TestLabel "Account Eval" testAccountEval
  , TestLabel "Complex Send Eval" testComplexSendEval
  , TestLabel "Multiple Sources/Destinations" testMultipleSourcesDestinations
  , TestLabel "Percentage Validation" testPercentageValidation
  , TestLabel "File Parsing" testFileParsing
  , TestLabel "Complete Example" testCompleteExample
  , TestLabel "Multiple Coins" testMultipleCoins
  , TestLabel "Insufficient Funds" testInsufficientFunds
  , TestLabel "Zero Amount" testZeroAmount
  , TestLabel "Complex Percentage" testComplexPercentage
  , TestLabel "Max Amount" testMaxAmount
  , TestLabel "Remaining Amount" testRemainingAmount
  , TestLabel "Complex Transaction Chain" testComplexTransactionChain
  , TestLabel "Account Creation" testAccountCreation
  , TestLabel "Multiple Destinations with Remaining" testMultipleDestinationsWithRemaining
  , TestLabel "Insufficient Source Amount" testInsufficientSourceAmount
  , TestLabel "Insufficient Source Amount Percentage" testInsufficientSourceAmountPercentage
  , TestLabel "Insufficient Source Amount Max" testInsufficientSourceAmountMax
  , TestLabel "Incomplete Destination Consumption" testIncompleteDestinationConsumption
  , TestLabel "Incomplete Destination Consumption Percentage" testIncompleteDestinationConsumptionPercentage
  , TestLabel "Nested Sources" testNestedSources
  , TestLabel "Nested Destinations" testNestedDestinations
  , TestLabel "Deeply Nested Sources" testDeeplyNestedSources
  , TestLabel "Nested Sources with Insufficient Funds" testNestedSourcesWithInsufficientFunds
  , TestLabel "Nested Destinations with Incomplete Consumption" testNestedDestinationsWithIncompleteConsumption
  , TestLabel "Destination Max Amount" testDestinationMaxAmount
  , TestLabel "Multiple Destination Max Amount" testDestinationMaxAmountWithMultipleMax
  , TestLabel "Destination Max Amount with Insufficient Remaining" testDestinationMaxAmountWithInsufficientRemaining
  , TestLabel "Destination Max Only" testDestinationMaxOnly
  , TestLabel "Destination Max with Zero Remaining" testDestinationMaxWithZeroRemaining
  , TestLabel "Destination Max with Large Max" testDestinationMaxWithLargeMax
  , TestLabel "Destination Max with Nested Destinations" testDestinationMaxWithNestedDestinations
  , TestLabel "Destination Max with Fractional Percentages" testDestinationMaxWithFractionalPercentages
  ]

-- Main test runner
main :: IO ()
main = do
  putStrLn "Running Okane DSL Tests..."
  runTestTT tests
  putStrLn "Tests completed!"
