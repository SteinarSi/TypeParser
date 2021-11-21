module Parser(parse) where

import Text.Parsec hiding (parse)
import Text.Parsec.Char (spaces)
import Text.ParserCombinators.Parsec.Char (char)

import Expr (Expr(..), VarName)

{-
Grammatikk:

        Lambda         Apply
Expr => \Var -> Expr | Function Argument | Var | (Expr)
Function => (Expr) | Var
Argument => (Expr) | Var
Var    => Letter*
Letter => a | b | ... | z | A | B | ... | Z
-}

parseTest :: String -> Parsec String [VarName] c -> Either ParseError c
parseTest text rule = runParser rule [] "(source)" text

parse :: String -> Either ParseError Expr
parse text = runParser expr [] "(source)" text

--ps <- many1 (parameter >>= \p -> spaces >> return p)
--modifyState (ps ++ )
--return (foldr Lambda e ps)
--f <- try (spaces >> function >>= \f -> spaces >> return f)
expr :: Parsec String [VarName] Expr
expr = try (do
        _ <- spaces
        _ <- char '\\'
        _ <- spaces
        ps <- many1 (parameter >>= \p -> spaces >> return p)
        _ <- spaces
        _ <- string "->"
        modifyState (ps ++ )
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
parameter = many1 (oneOf legalLetters)
    
boundedVar :: Parsec String [VarName] Expr
boundedVar = try (do
        cs <- many1 (oneOf legalLetters)
        boundedvars <- getState
        if elem cs boundedvars then return (Var cs)
        else fail ("unexpected free variable '" ++ cs ++ "'")
    )

legalLetters :: [Char]
legalLetters = ['a'..'z'] ++ ['A'..'Z']
