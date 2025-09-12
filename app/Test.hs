{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Lang
import Parse (runP, term, parseFile)
import Eval (runEval, runEvalWithState, runEvalStatements, showAccounts, initAccount, initialState, EvalState(..))
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
mkState accountsList = EvalState $ Map.fromList $ map (\(name, coin, amount) -> (name, Map.singleton coin amount)) accountsList

-- Basic parsing tests
testBasicSend :: Test
testBasicSend = TestCase $ do
  assertParse "send alice [USD 100] bob" 
    (SSend [SAcc "alice"] (Coin "USD" 100) [DAcc "bob"])
  
testTransaction :: Test
testTransaction = TestCase $ do
  assertParse "trx { send alice [USD 100] bob; send bob [USD 50] charlie }"
    (STrx [SSend [SAcc "alice"] (Coin "USD" 100) [DAcc "bob"],
           SSend [SAcc "bob"] (Coin "USD" 50) [DAcc "charlie"]])

-- Basic evaluation tests
testBasicSendEval :: Test
testBasicSendEval = TestCase $ do
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500)]
  let stm = SSend [SAcc "alice"] (Coin "USD" 100) [DAcc "bob"]
  case runEvalWithState stm initState of
    Right finalState -> do
      let aliceBalance = case Map.lookup "alice" (accounts finalState) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" (accounts finalState) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD" 900 aliceBalance
      assertEqual "Bob should have 600 USD" 600 bobBalance
    Left err -> assertFailure ("Basic send should work: " ++ show err)

testTransactionEval :: Test
testTransactionEval = TestCase $ do
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500), ("charlie", "USD", 200)]
  let stm = STrx [SSend [SAcc "alice"] (Coin "USD" 100) [DAcc "bob"], 
                  SSend [SAcc "bob"] (Coin "USD" 50) [DAcc "charlie"]]
  case runEvalWithState stm initState of
    Right finalState -> do
      let aliceBalance = case Map.lookup "alice" (accounts finalState) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance = case Map.lookup "bob" (accounts finalState) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance = case Map.lookup "charlie" (accounts finalState) of
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
          let aliceBalance = case Map.lookup "alice" (accounts result) of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          let bobBalance = case Map.lookup "bob" (accounts result) of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          let charlieBalance = case Map.lookup "charlie" (accounts result) of
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
      let aliceBalance = case Map.lookup "alice" (accounts result) of
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
          let aliceBalance = case Map.lookup "alice" (accounts result) of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          let bobBalance = case Map.lookup "bob" (accounts result) of
                Just coins -> Map.findWithDefault 0 "USD" coins
                Nothing -> 0
          assertEqual "Alice should have 900 USD after complete example" 900 aliceBalance
          assertEqual "Bob should have 600 USD after complete example" 600 bobBalance
        Left err -> assertFailure $ "Complete example execution failed: " ++ show err
    Left err -> assertFailure $ "Complete example parsing failed: " ++ show err

testComplexSendParse :: Test
testComplexSendParse = TestCase $ do
  -- Test basic percentage parsing (single source)
  assertParse "send {50% from alice} [USD 100] bob" (SSend [SPerc (Perc 50) [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"])
  -- Test basic max parsing
  assertParse "send {max 100 from alice} [USD 100] bob" (SSend [SMax 100 [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"])
  -- Test basic remaining parsing
  assertParse "send {remaining from alice} [USD 100] bob" (SSend [SRem [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"])

testComplexSendEval :: Test
testComplexSendEval = TestCase $ do
  -- Test percentage send (100% of 100 = 100)
  let stm1 = SSend [SPerc (Perc 100) [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState1 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm1 initState1 of
    Right result1 -> do
      let aliceBalance1 = case Map.lookup "alice" (accounts result1) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance1 = case Map.lookup "bob" (accounts result1) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after 100% of 100 send" 900 aliceBalance1
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance1
    Left err -> assertFailure $ "Percentage send failed: " ++ show err

  -- Test fraction send (1/1 of 100 = 100%)
  let stm2 = SSend [SPerc (Rat 1 1) [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState2 = mkState [("alice", "USD", 900), ("bob", "USD", 0)]
  case runEvalWithState stm2 initState2 of
    Right result2 -> do
      let aliceBalance2 = case Map.lookup "alice" (accounts result2) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance2 = case Map.lookup "bob" (accounts result2) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 800 USD after 1/1 of 100 send" 800 aliceBalance2
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance2
    Left err -> assertFailure $ "Fraction send failed: " ++ show err

  -- Test max send (max 200, but only send 100)
  let stm3 = SSend [SMax 200 [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState3 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm3 initState3 of
    Right result3 -> do
      let aliceBalance3 = case Map.lookup "alice" (accounts result3) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance3 = case Map.lookup "bob" (accounts result3) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after max send" 900 aliceBalance3
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance3
    Left err -> assertFailure $ "Max send failed: " ++ show err

  -- Test remaining send (remaining = 100)
  let stm4 = SSend [SRem [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState4 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm4 initState4 of
    Right result4 -> do
      let aliceBalance4 = case Map.lookup "alice" (accounts result4) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance4 = case Map.lookup "bob" (accounts result4) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after remaining send" 900 aliceBalance4
      assertEqual "Bob should have 100 USD after receiving" 100 bobBalance4
    Left err -> assertFailure $ "Remaining send failed: " ++ show err

testMultipleSourcesDestinations :: Test
testMultipleSourcesDestinations = TestCase $ do
  -- Test multiple sources with percentage (100% of 100 = 100, split between 2 sources = 50 each)
  let stm1 = SSend [SPerc (Perc 100) [SAcc "alice", SAcc "bob"]] (Coin "USD" 100) [DAcc "charlie"]
  let initState1 = mkState [("alice", "USD", 1000), ("bob", "USD", 800), ("charlie", "USD", 0)]
  case runEvalWithState stm1 initState1 of
    Right result1 -> do
      let aliceBalance1 = case Map.lookup "alice" (accounts result1) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance1 = case Map.lookup "bob" (accounts result1) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance1 = case Map.lookup "charlie" (accounts result1) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 950 USD after 100% of 100 send" 950 aliceBalance1
      assertEqual "Bob should have 750 USD after 100% of 100 send" 750 bobBalance1
      assertEqual "Charlie should have 100 USD after receiving" 100 charlieBalance1
    Left err -> assertFailure $ "Multiple sources percentage send failed: " ++ show err

  -- Test multiple destinations (each gets full amount)
  let stm2 = SSend [SAcc "alice"] (Coin "USD" 100) [DAcc "bob", DAcc "charlie"]
  let initState2 = mkState [("alice", "USD", 1000), ("bob", "USD", 0), ("charlie", "USD", 0)]
  case runEvalWithState stm2 initState2 of
    Right result2 -> do
      let aliceBalance2 = case Map.lookup "alice" (accounts result2) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance2 = case Map.lookup "bob" (accounts result2) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance2 = case Map.lookup "charlie" (accounts result2) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 900 USD after sending to multiple destinations" 900 aliceBalance2
      assertEqual "Bob should have 100 USD after receiving full amount" 100 bobBalance2
      assertEqual "Charlie should have 100 USD after receiving full amount" 100 charlieBalance2
    Left err -> assertFailure $ "Multiple destinations send failed: " ++ show err

  -- Test complex case: multiple sources with max, multiple destinations
  let stm3 = SSend [SMax 150 [SAcc "alice", SAcc "bob"]] (Coin "USD" 100) [DAcc "charlie", DAcc "david"]
  let initState3 = mkState [("alice", "USD", 1000), ("bob", "USD", 800), ("charlie", "USD", 0), ("david", "USD", 0)]
  case runEvalWithState stm3 initState3 of
    Right result3 -> do
      let aliceBalance3 = case Map.lookup "alice" (accounts result3) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let bobBalance3 = case Map.lookup "bob" (accounts result3) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let charlieBalance3 = case Map.lookup "charlie" (accounts result3) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      let davidBalance3 = case Map.lookup "david" (accounts result3) of
            Just coins -> Map.findWithDefault 0 "USD" coins
            Nothing -> 0
      assertEqual "Alice should have 950 USD after max send" 950 aliceBalance3
      assertEqual "Bob should have 750 USD after max send" 750 bobBalance3
      assertEqual "Charlie should have 100 USD after receiving full amount" 100 charlieBalance3
      assertEqual "David should have 100 USD after receiving full amount" 100 davidBalance3
    Left err -> assertFailure $ "Complex multiple sources/destinations send failed: " ++ show err

testPercentageValidation :: Test
testPercentageValidation = TestCase $ do
  -- Test valid 100% percentage
  let stm1 = SSend [SPerc (Perc 100) [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState1 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm1 initState1 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "100% percentage should be valid: " ++ show err

  -- Test invalid percentage (75% doesn't add up to 100%)
  let stm2 = SSend [SPerc (Perc 75) [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState2 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm2 initState2 of
    Right _ -> assertFailure "75% percentage should be invalid"
    Left (E.InvalidPercentageSum 75 100) -> return () -- Should fail with correct error
    Left err -> assertFailure $ "Expected InvalidPercentageSum error, got: " ++ show err

  -- Test valid max amount (100% of transfer amount)
  let stm3 = SSend [SMax 100 [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState3 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm3 initState3 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "Max 100 should be valid: " ++ show err

  -- Test valid remaining (always valid)
  let stm4 = SSend [SRem [SAcc "alice"]] (Coin "USD" 100) [DAcc "bob"]
  let initState4 = mkState [("alice", "USD", 1000), ("bob", "USD", 0)]
  case runEvalWithState stm4 initState4 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "Remaining should be valid: " ++ show err

  -- Test valid combination: 50% + max 50 (100% total)
  let stm5 = SSend [SPerc (Perc 50) [SAcc "alice"], SMax 50 [SAcc "bob"]] (Coin "USD" 100) [DAcc "charlie"]
  let initState5 = mkState [("alice", "USD", 1000), ("bob", "USD", 1000), ("charlie", "USD", 0)]
  case runEvalWithState stm5 initState5 of
    Right _ -> return () -- Should succeed
    Left err -> assertFailure $ "50% + max 50 should be valid: " ++ show err

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
  ]

-- Main test runner
main :: IO ()
main = do
  putStrLn "Running Okane DSL Tests..."
  runTestTT tests
  putStrLn "Tests completed!"
