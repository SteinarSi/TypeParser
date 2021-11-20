module Expr(Expr(..), VarName) where

data Expr = Lambda VarName Expr
          | Apply  Expr Expr
          | Var    VarName
    deriving Eq

type VarName = String

-- "\x -> \y -> x y" => Lambda "x" (Lambda "y" (Apply (Var "x") (Var "y")))

instance Show Expr where
    show (Var s)                   = s
    show (Lambda p e)              = "\\" ++ p ++ " -> " ++ show e
    show (Apply (Var f) (Var arg)) = f ++ " " ++ arg
    show (Apply (Var f) arg)       = f ++ " (" ++ show arg ++ ")"
    show (Apply f       (Var arg)) = "(" ++ show f ++ ") " ++ arg
    show (Apply f       arg)       = "(" ++ show f ++ ") (" ++ show arg ++ ")"