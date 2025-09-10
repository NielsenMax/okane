module Main where

import System.Console.Haskeline
import Parse (runP, term, parseFile)
import Eval (runEval, runEvalWithState, runEvalStatements, showAccounts, initAccount, initialState, EvalState(..), saveAccountsToDSLFile)
import Data.List (isPrefixOf)
import System.IO (readFile)
import Control.Exception (catch, IOException)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = runInputT defaultSettings (loop initialState)
  where
    loop :: EvalState -> InputT IO ()
    loop state = do
      minput <- getInputLine "okane> "
      case minput of
        Nothing -> outputStrLn "Goodbye!"
        Just input -> do
          case input of
            "" -> loop state
            ":help" -> do
              outputStrLn "Okane DSL - Financial Transaction Language"
              outputStrLn "Commands:"
              outputStrLn "  :help     - Show this help"
              outputStrLn "  :quit     - Exit the interpreter"
              outputStrLn "  :accounts - Show current account balances"
              outputStrLn "  :load <file> - Load and execute a file"
              outputStrLn "  :save <file> - Save current account balances to a file"
              outputStrLn ""
              outputStrLn "Transaction syntax:"
              outputStrLn "  account <name> <coin> <amount>"
              outputStrLn "  send <sources> <coin> <destinations>"
              outputStrLn "  trx { <statements> }"
              outputStrLn ""
              outputStrLn "Examples:"
              outputStrLn "  account alice USD 1000"
              outputStrLn "  send alice [USD 100] bob"
              outputStrLn "  send 50% from {alice, bob} [BTC 1] charlie"
              outputStrLn "  trx { send alice [USD 50] bob; send bob [USD 25] charlie }"
              outputStrLn "  :load transactions.okane"
              loop state
            ":quit" -> outputStrLn "Goodbye!"
            ":accounts" -> do
              outputStrLn "Current account balances:"
              outputStrLn $ showAccounts (accounts state)
              loop state
            _ -> do
              if ":load " `isPrefixOf` input
                then do
                  let filename = drop 6 input  -- Remove ":load " prefix
                  result <- liftIO $ catch (do
                    content <- readFile filename
                    return $ Right content
                    ) (\e -> return $ Left (e :: IOException))
                  case result of
                    Left ioErr -> do
                      outputStrLn $ "✗ Error reading file: " ++ show ioErr
                      loop state
                    Right content -> do
                      case parseFile content of
                        Right statements -> do
                          outputStrLn $ "✓ Loaded " ++ show (length statements) ++ " statements from " ++ filename
                          case runEvalStatements statements state of
                            Right newState -> do
                              outputStrLn "✓ File executed successfully!"
                              outputStrLn "Updated account balances:"
                              outputStrLn $ showAccounts (accounts newState)
                              loop newState
                            Left err -> do
                              outputStrLn "✗ Execution error:"
                              outputStrLn $ "  " ++ show err
                              loop state
                        Left err -> do
                          outputStrLn "✗ Parse error in file:"
                          outputStrLn $ "  " ++ show err
                          loop state
                else if ":save " `isPrefixOf` input
                  then do
                    let filename = drop 6 input  -- Remove ":save " prefix
                    result <- liftIO $ saveAccountsToDSLFile filename (accounts state)
                    case result of
                      Right _ -> do
                        outputStrLn $ "✓ Accounts saved to " ++ filename ++ " in DSL format"
                        loop state
                      Left err -> do
                        outputStrLn $ "✗ Error saving accounts: " ++ err
                        loop state
                else do
                      case runP term input "" of
                        Right parsed -> do
                          outputStrLn "✓ Parsed successfully:"
                          outputStrLn $ "  " ++ show parsed
                          case runEvalWithState parsed state of
                            Right newState -> do
                              outputStrLn "✓ Executed successfully!"
                              outputStrLn "Updated account balances:"
                              outputStrLn $ showAccounts (accounts newState)
                              loop newState
                            Left err -> do
                              outputStrLn "✗ Execution error:"
                              outputStrLn $ "  " ++ show err
                              loop state
                        Left err -> do
                          outputStrLn "✗ Parse error:"
                          outputStrLn $ "  " ++ show err
                          loop state
