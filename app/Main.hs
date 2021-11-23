module Main where

import Unification (martelliMontanari)
import TypeDerivation (hindleyMilner)
import Parser (parse)


main :: IO ()
main = do
    putStrLn "\n\nWrite a lambda expression to continue:\n"
    inn <- getLine
    case parse inn of
        Left err -> print err >> main
        Right ex -> do
            putStrLn ("\nI parsed your expression like this: \n" ++ show ex ++ "\n\n")
            let (lines, eqs) = hindleyMilner ex
            mapM_ putStrLn lines
            putStrLn ("\nType derivation resulted in this equation set: \n" ++ show eqs ++ "\n\nUnification:")
            let ms = martelliMontanari eqs
            mapM_ putStrLn ms
            main
