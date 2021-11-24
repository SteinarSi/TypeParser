module TestTypeDerivation (testTypeDerivation) where

import Test.HUnit (Test(..), assertEqual)

import TypeDerivation

testTypeDerivation :: Test
testTypeDerivation = TestList [testSimplify, recursiveSimplify]


testSimplify :: Test
testSimplify = TestCase (assertEqual "t = e -> r -> e" 
    (Equation [UType 't'] [UType 'a', UType 'b', UType 'a']) 
    (simplify (Equation [UType 't'] [UType 'e', UType 'r', UType 'e'])))

recursiveSimplify :: Test
recursiveSimplify = TestCase (assertEqual "t = (e -> e -> r -> (w -> x) -> x) -> r -> e"
    (Equation [UType 't'] [NType [UType 'a', UType 'a', UType 'b', NType [UType 'c', UType 'd'], UType 'd'], UType 'b', UType 'a'])
    (simplify (Equation [UType 't'] [NType [UType 'e', UType 'e', UType 'r', NType [UType 'w', UType 'x'], UType 'x'], UType 'r', UType 'e'])))