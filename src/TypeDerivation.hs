module TypeDerivation(Type(..), Equation(..), hindleyMilner) where

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.List (find)
import Data.Maybe (fromJust)

import Expr
import Parser


type TypeBank s = STRef s [Type]
data Type       = UType Char | NType [Type] deriving Eq
data Param      = Param String Type         deriving Eq
data Equation   = Equation [Type] [Type]    deriving Eq


instance Show Type where
    show (UType c) = [c]
    show (NType ts) = showList ts ""
    showList [UType c] = (++[c])
    showList (UType c : ts) = (++ showList ts (c : " -> "))
    showList [NType ts] = (++ showList ts "(" ++ ")")
    showList ((NType ts):tss) = (++ showList tss (showList ts "(" ++ ") -> "))

instance Show Equation where
    show (Equation a b) = showList a "" ++ " = " ++ showList b ""


typeBank :: [Type]
typeBank = map UType $ ['a'..'s'] ++ ['u'..'z'] ++ "æøå" ++ ['A'..'S'] ++ ['U'..'Z'] ++ "ÆØÅ" -- ++ "αβγδεζηθΙικλμνξπρσςτυφχψω" ++ "ΓΔΘΙΛΝΞΠΡΣΤΥΦΧΨΩ"


hindleyMilner :: Expr -> ([String], [Equation])
hindleyMilner e = runST $ do
    bank <- newSTRef typeBank 
    lines <- newSTRef [show e ++ " :: t"]
    eqs <- derive e [] [UType 't'] bank lines
    ls <- readSTRef lines
    return (reverse ls, eqs)

derive :: Expr -> [Param] -> [Type] -> STRef s [Type] -> STRef s [String] -> ST s [Equation]
derive (Var x) ps ts bank lines = do
    let Param _ tp = fromJust (find (\(Param name _) -> name == x) ps)
    modifySTRef' lines (("(T2) " ++ show tp ++ " = " ++ show ts) :)
    return [Equation [tp] ts]
derive (Lambda p e) ps ts bank lines = do
    (t1:t2:_) <- drawTypes 2 bank
    modifySTRef' lines (("(T4) " ++ p ++ " :: " ++ show t1 ++ " | " ++ show e ++ " :: " ++ show t2 ++ "\t\t" ++ show ts ++ " = " ++ show t1 ++ " -> " ++ show t2) : )
    eqs <- derive e (Param p t1 : ps) [t2] bank lines
    return ( Equation ts [t1, t2] : eqs)
derive (Apply f arg) ps ts bank lines = do
    (t1:_) <- drawTypes 1 bank
    modifySTRef' bank tail
    modifySTRef' lines (("(T3) " ++ show arg ++ " :: " ++ show t1) : )
    modifySTRef' lines (("     " ++ show f   ++ " :: " ++ show (t1 : ts)) : )
    eqs1 <- derive arg ps [t1] bank lines
    eqs2 <- derive f ps (t1 : ts) bank lines
    return (eqs1 ++ eqs2)

drawTypes :: Int -> TypeBank s -> ST s [Type]
drawTypes 0 _ = return []
drawTypes n bank = do
    t  <- liftM head (readSTRef bank)
    modifySTRef' bank tail
    ts <- drawTypes (n-1) bank
    return (t:ts)

