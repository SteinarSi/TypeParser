module Main where

import Test.HUnit

import TestParser (testParser)
import TestTypeDerivation (testTypeDerivation)

allTests :: Test
allTests = TestList [testParser, testTypeDerivation]

main :: IO Counts
main = runTestTT allTests