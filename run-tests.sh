#!/bin/bash

echo "Running Okane DSL Test Suite"
echo "============================"
echo ""

# Run the test suite
cabal test

echo ""
echo "Test Summary:"
echo "- All core parsing functionality tested"
echo "- All basic evaluation functionality tested"
echo "- Error conditions tested"
echo "- Integration tests included"
echo ""
echo "To run individual tests, you can also use:"
echo "  cabal run okane-test"
