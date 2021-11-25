module Parser (parse) where

import Text.Parsec hiding (parse)
import Text.Parsec.Char (spaces)
import Text.ParserCombinators.Parsec.Char (char)

import Expr (Expr(..), VarName)

{-
Grammatikk:

        Lambda         Apply
Expr => \Var -> Expr | Function Argument* | Var | (Expr)
Function => (Expr) | Var
Argument => (Expr) | Var
Var    => Letter*
Letter => a | b | ... | z | A | B | ... | Z
-}

parse :: String -> Either ParseError Expr
parse = runParser ((eof >>) . return =<< expr) [] "(source)"

expr :: Parsec String [VarName] Expr
expr = try (do
        _ <- spaces
        _ <- char '\\'
        _ <- spaces
        ps <- many1 (parameter >>= \p -> spaces >> modifyState (p :) >> return p)
        _ <- spaces
        _ <- string "->"
        _ <- spaces
        e <- expr
        return (foldr Lambda e ps)
    ) <|> do
        f <- function
        args <- many (try (spaces >> argument))
        _ <- spaces
        return (foldl Apply f args)

function :: Parsec String [VarName] Expr
function = argument

argument :: Parsec String [VarName] Expr
argument = try (do
        char '('
        _ <- spaces
        e <- expr
        _ <- spaces
        char ')'
        return e
    ) <|> boundedVar

parameter :: Parsec String [VarName] VarName
parameter = do
    cs <- many1 (oneOf legalLetters)
    boundedVars <- getState
    if elem cs boundedVars then fail ("Ambiguous reference to variable " ++ cs ++ ", which was bound by multiple lambdas")
    else return cs
    
boundedVar :: Parsec String [VarName] Expr
boundedVar = try (do
        cs <- many1 (oneOf legalLetters)
        boundedvars <- getState
        if elem cs boundedvars then return (Var cs)
        else fail ("unexpected free variable '" ++ cs ++ "'")
    )

legalLetters :: [Char]
legalLetters = ['a'..'z'] ++ "æøå" ++ ['A'..'Z'] ++ "ÆØÅ"
