module Unification(martelliMontanari) where

import TypeDerivation (Equation(..), Type(..), simplify)
import Data.List (find, delete)
import Data.Maybe (listToMaybe, catMaybes)

data URule = U1 | U2 | U3 | U4 | U5 | U6 deriving Show

martelliMontanari :: [Equation] -> [String]
martelliMontanari eqs = show eqs : case findRule eqs of
        Nothing -> "\nFinished!" : [show (simplify (head eqs))]
        Just (U6, eq) ->  ["\nOccurs check, cannot construct the infinite type " ++ show eq ++ ".", "This expression is therefore misstyped."]
        Just (r,  eq) -> ("\nRule " ++ show r ++ " on " ++ show eq ++ ":") : martelliMontanari (applyRule eqs eq r)

applyRule :: [Equation] -> Equation -> URule -> [Equation]
applyRule eqs eq U1 = delete eq eqs
applyRule eqs eq U2 = delete eq eqs ++ map lower (zipEquation eq)
applyRule eqs eq@(Equation a b) U4 = delete eq eqs ++ [Equation b a]
applyRule eqs eq U5 = map lower (replace eqs eq)


findRule :: [Equation] -> Maybe (URule, Equation)
findRule eqs = listToMaybe $ catMaybes (map (\(r, f) -> f eqs >>= Just . (,) r) [(U1, findU1), (U6, findU6), (U2, findU2), (U4, findU4), (U5, findU5)])

findU1 :: [Equation] -> Maybe Equation
findU1 = find (\(Equation a b) -> a == b)

findU6 :: [Equation] -> Maybe Equation
findU6 = find (\(Equation a b) -> length a == 1 && contains b (head a))

findU2 :: [Equation] -> Maybe Equation
findU2 = find (\(Equation a b) -> length a > 1 && length b > 1)

findU4 :: [Equation] -> Maybe Equation
findU4 = find (\(Equation a b) -> length a > 1 && length b == 1)

findU5 :: [Equation] -> Maybe Equation
findU5 = find (\(Equation a b) -> head a /= UType 't')


contains :: [Type] -> Type -> Bool
contains [] _ = False
contains ((NType ts):tss) t = contains ts t || contains tss t
contains (t1:tss) t2 = t1 == t2 || contains tss t2

--U2
zipEquation :: Equation -> [Equation]
zipEquation (Equation [a] b)  = [Equation [a] b ]
zipEquation (Equation  a [b]) = [Equation  a [b]]
zipEquation (Equation (a:as) (b:bs)) = Equation [a] [b] : zipEquation (Equation as bs)
zipEquation (Equation _ _)  = error "One of the lists was empty when it shouldn't have been"

--U5
replace :: [Equation] -> Equation -> [Equation]
replace eqs (Equation [UType a] b) = map (\(Equation a2 b2) -> Equation (replace'' a2) (replace'' b2)) eqs
    where replace' (NType ts) = NType (map replace' ts)
          replace' (UType t) | t == a = replacement
                             | otherwise = UType t
          replacement | length b == 1 = head b
                      | otherwise = NType b

          replace'' [] = []
          replace'' [UType t] | t == a = b
                              | otherwise = [UType t]
          replace'' (UType t : ts) | t == a && length b > 1 = NType b : replace'' ts
                                   | t == a = head b : replace'' ts
                                   | otherwise = UType t : replace'' ts
          replace'' (NType ts : tss) = NType (replace'' ts) : replace'' tss
 
--Dette dekker en edge case i U2 og U5. Om vi f. eks har (a -> b) -> c = e -> d, får vi blant annet (a -> b) = e,
-- og da vil dette på en måte fjerne parantesene så vi får a -> b = e .
lower :: Equation -> Equation
lower (Equation a b) = Equation (lower' a) (lower' b)
    where lower' [NType bs] = bs
          lower' ts = ts