module TestParser (testParser) where

import Test.HUnit
import Expr (Expr(..))
import Parser (parse)
import Data.Either (isRight, isLeft)

testParser :: Test
testParser = TestList [justX, justXNoSpaces, justXTooManySpaces, xAndY, shortForm, xAppliedToY, bullshit, freeVariable,
                       xyzuvApplied, xyzuvWithMoreParantheses, xyzuvNoSpaces, xyzuvWithMoreSpaces]

justX, justXNoSpaces, justXTooManySpaces :: Test
justX = TestCase (assertEqual "Just \\x -> x" (Right (Lambda "x" (Var "x"))) (parse "\\x -> x"))
justXNoSpaces = TestCase (assertEqual "Just \\x -> x, no spaces though" (Right (Lambda "x" (Var "x"))) (parse "\\x->x"))
justXTooManySpaces = TestCase (assertEqual "Just \\x -> x, but with loads of spaces" (Right (Lambda "x" (Var "x"))) (parse "    \\   x   ->    x   "))

xAndY, xAppliedToY, shortForm :: Test
xAndY = TestCase (assertEqual "\\x -> \\y -> x" (Right (Lambda "x" (Lambda "y" (Var "x")))) (parse "\\x -> \\y -> x"))
xAppliedToY = TestCase (assertEqual "\\x -> \\y -> x y" (Right (Lambda "x" (Lambda "y" (Apply (Var "x") (Var "y"))))) (parse "\\x -> \\y -> x y"))
shortForm = TestCase (assertEqual "\\x y -> x y" (Right (Lambda "x" (Lambda "y" (Apply (Var "x") (Var "y"))))) (parse "\\x y -> x y"))

bullshit, freeVariable :: Test
bullshit = TestCase (assertBool "asdfghjkl" (isLeft (parse "asdfghjkl")))
freeVariable = TestCase (assertBool "\\x -> y" (isLeft (parse "\\x -> y")))

xyzuvApplied, xyzuvWithMoreParantheses, xyzuvNoSpaces, xyzuvWithMoreSpaces :: Test
xyzuvApplied = TestCase (assertEqual "\\x y z u v -> x y z u v" (Right (Lambda "x" (Lambda "y" (Lambda "z" (Lambda "u" (Lambda "v" (Apply (Apply (Apply (Apply (Var "x") (Var "y")) (Var "z")) (Var "u")) (Var "v")))))))) (parse "\\x y z u v -> x y z u v"))
xyzuvWithMoreParantheses = TestCase (assertEqual "\\x y z u v -> (((x y) z) u v)" (Right (Lambda "x" (Lambda "y" (Lambda "z" (Lambda "u" (Lambda "v" (Apply (Apply (Apply (Apply (Var "x") (Var "y")) (Var "z")) (Var "u")) (Var "v")))))))) (parse "\\x y z u v -> (((x y) z) u v)"))
xyzuvNoSpaces = TestCase (assertEqual "\\x y z u v->(((x y)z)u)v" (Right (Lambda "x" (Lambda "y" (Lambda "z" (Lambda "u" (Lambda "v" (Apply (Apply (Apply (Apply (Var "x") (Var "y")) (Var "z")) (Var "u")) (Var "v")))))))) (parse "\\x y z u v->(((x y)z)u)v"))
xyzuvWithMoreSpaces = TestCase (assertEqual "\\x y  z u v  -> ( ( ( x    y )  z  )   u  v   ) " (Right (Lambda "x" (Lambda "y" (Lambda "z" (Lambda "u" (Lambda "v" (Apply (Apply (Apply (Apply (Var "x") (Var "y")) (Var "z")) (Var "u")) (Var "v")))))))) (parse "\\x y  z u v  -> ( ( ( x    y )  z  )   u  v   ) "))










