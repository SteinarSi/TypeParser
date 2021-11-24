module TypeDerivation(Type(..), Equation(..), hindleyMilner, simplify) where

import Control.Monad.ST (ST, runST)
import Control.Monad (liftM, when)
import Data.STRef (STRef, newSTRef, readSTRef, modifySTRef')
import Data.List (find, foldl')
import Data.Maybe (fromJust)

import Expr (Expr(..))


type TypeBank s = STRef s [Type]
data Type       = UType Char | NType [Type] deriving Eq
data Param      = Param String Type         deriving Eq
data Equation   = Equation [Type] [Type]    deriving Eq
data TRule      = T2 Type [Type] | T3 Expr Expr Type [Type] | T4 String Type Expr Type [Type]


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
    let initial = show e ++ " :: t"
    lines <- newSTRef []
    eqs <- derive e [] [UType 't'] bank lines (length initial + 20)
    ls <- readSTRef lines
    return (reverse ("Type derivation:" : (show e ++ " :: t") : map (draw (length initial  + 20)) ls), eqs)

derive :: Expr -> [Param] -> [Type] -> TypeBank s -> STRef s [TRule] -> Int -> ST s [Equation]
derive (Var x) ps ts bank lines len = do
    let Param _ tp = fromJust (find (\(Param name _) -> name == x) ps)
    modifySTRef' lines (T2 tp ts : )
    return [Equation [tp] ts]
derive (Lambda p e) ps ts bank lines len = do
    t1 <- drawType bank
    t2 <- drawType bank
    modifySTRef' lines (T4 p t1 e t2 ts : )
    eqs <- derive e (Param p t1 : ps) [t2] bank lines len
    return ( Equation ts [t1, t2] : eqs)
derive (Apply f arg) ps ts bank lines len = do
    t1 <- drawType bank
    modifySTRef' lines (T3 f arg t1 ts :)
    eqs1 <- derive arg ps [t1] bank lines len
    eqs2 <- derive f ps (t1 : ts) bank lines len
    return (eqs1 ++ eqs2)

drawType :: TypeBank s -> ST s Type
drawType bank = do
    t  <- readSTRef bank
    when (null t) (error "Sorry, we ran out of type variable names! This expression is too large, please give me a smaller expression instead.")
    modifySTRef' bank tail
    return (head t)


--data TRule = T2 Type [Type] | T3 Expr Expr Type [Type] | T4 String Type Expr Type [Type]
draw :: Int -> TRule -> String
draw n (T2 a ts) = "(T2)" ++ replicate (n-4) ' ' ++ show a ++ " = " ++ show ts
draw n (T3 f arg t ts) = let m = max (length (show f)) (length (show arg))
                         in  "(T3) | " ++ show arg ++ replicate (m - length (show arg)) ' ' ++ " :: " ++ show t
                        ++ "\n     | " ++ show f   ++ replicate (m - length (show f  )) ' ' ++ " :: " ++ show (t:ts)
draw n (T4 p t1 e t2 ts) = let line = "(T4) " ++ show p ++ " :: " ++ show t1 ++ " | " ++ show e ++ " :: " ++ show t2
                           in  line ++ replicate (n - length line) ' ' ++ show ts ++ " = " ++ show t1 ++ " -> " ++ show t2

simplify :: Equation -> Equation
simplify (Equation a b) = let (_, _, NType nt) = simplify' typeBank [] (NType b)
                          in  (Equation a nt)
    where
        simplify' :: [Type] -> [(Type, Type)] -> Type -> ([Type], [(Type, Type)], Type)
        simplify' bank tmap t@(UType _) = case lookup t tmap of
            Nothing -> (tail bank, (t, head bank) : tmap, head bank)
            Just t0 -> (bank, tmap, t0)
        simplify' bank tmap (NType ts) = let (newbank, newmap, newts) = foldl' (\(b, tm, types) t -> 
                                                        let (nb, ntm, nt) = simplify' b tm t 
                                                        in  (nb, ntm, nt : types)
                                                    ) (bank, tmap, []) ts
                                         in  (newbank, newmap, NType (reverse newts))