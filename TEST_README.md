# Okane DSL Test Suite

This directory contains a comprehensive test suite for the Okane DSL (Domain Specific Language for financial transactions).

## Running Tests

### Quick Start
```bash
# Run all tests
./run-tests.sh

# Or using cabal directly
cabal test
```

### Individual Test Execution
```bash
# Run the test executable directly
cabal run okane-test
```

## Test Coverage

The test suite covers the following areas:

### 1. Parsing Tests
- **Basic Send Operations**: `send alice [USD 100] bob`
- **Function Definitions**: `fun transfer alice src { send alice [USD 100] bob }`
- **Transaction Blocks**: `trx { send alice [USD 100] bob }`
- **Function Calls**: `run transfer alice`

### 2. Evaluation Tests
- **Basic Transfers**: Account balance updates
- **Insufficient Funds**: Error handling for insufficient balances
- **Remaining Transfers**: `remaining from` source evaluation
- **Function Storage**: Function definition without execution
- **Transaction Block Execution**: Multiple statements in sequence
- **Multiple Coins**: Handling different currency types

### 3. Integration Tests
- **Parse and Eval**: End-to-end parsing and evaluation
- **State Management**: Account balance persistence
- **Error Propagation**: Proper error handling throughout

## Test Structure

### Test Utilities
- `assertParse`: Validates parsing results
- `assertParseError`: Validates parse error conditions
- `assertEval`: Validates evaluation results
- `assertEvalError`: Validates evaluation error conditions
- `mkState`: Helper to create initial evaluation state

### Test Categories
1. **Parsing Tests**: Verify AST generation from source code
2. **Evaluation Tests**: Verify execution semantics
3. **Integration Tests**: Verify end-to-end functionality

## Example Test Cases

```haskell
-- Basic transfer test
testBasicTransfer :: Test
testBasicTransfer = TestCase $ do
  let stm = SSend [SAcc "alice"] (Coin "USD" 100) [DAcc "bob"]
  let initState = mkState [("alice", "USD", 1000), ("bob", "USD", 500)]
  let expectedState = mkState [("alice", "USD", 900), ("bob", "USD", 600)]
  assertEval stm initState expectedState
```

## Adding New Tests

To add new tests:

1. **Create a test function**:
   ```haskell
   testNewFeature :: Test
   testNewFeature = TestCase $ do
     -- Your test logic here
   ```

2. **Add to the test suite**:
   ```haskell
   tests :: Test
   tests = TestList
     [ -- existing tests...
     , TestLabel "New Feature" testNewFeature
     ]
   ```

3. **Run tests** to verify:
   ```bash
   cabal test
   ```

## Dependencies

The test suite uses:
- **HUnit**: Unit testing framework
- **Test.HUnit**: Test assertions and utilities
- **Lang**: Language AST definitions
- **Parse**: Parser functions
- **Eval**: Evaluator functions

## Test Results

All tests should pass with the current implementation. The test suite validates:
- ✅ Basic transaction parsing and execution
- ✅ Function definition and storage
- ✅ Transaction block execution
- ✅ Error handling for insufficient funds
- ✅ Multiple coin type support
- ✅ State management and persistence

## Future Enhancements

Potential areas for test expansion:
- Complex percentage calculations
- Multi-source/multi-destination transfers
- Function parameter binding
- Advanced error conditions
- Performance tests for large transactions
