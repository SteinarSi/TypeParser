module Main where

import Test.HUnit

import TestParser (testParser)

main :: IO Counts
main = runTestTT testParser