{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Arrows #-}

module Main where

import Lib

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

data LispVal 
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

symbol ∷ Parser Char
symbol = 
    oneOf "!#$%&|*+-/:<=>?@^_~"

spaces ∷ Parser ()
spaces = 
    skipMany1 space

parseAtom ∷ Parser LispVal
parseAtom = 
    do 
        first ← letter <|> symbol
        rest ← many (letter <|> digit <|> symbol)
        let atom = first:rest
        return $ case atom of 
            "#t" → Bool True

            "#f" → Bool False

            _    → Atom atom

parseList ∷ Parser LispVal
parseList = 
    liftM List $ sepBy parseExpr spaces

parseDottedList ∷ Parser LispVal
parseDottedList = 
    do
        head ← endBy parseExpr spaces
        tail ← char '.' >> spaces >> parseExpr
        return $ DottedList head tail

parseNumber ∷ Parser LispVal
parseNumber = 
    fmap (Number . read) (many1 digit)

parseQuoted ∷ Parser LispVal
parseQuoted = 
    do
        char '\''
        x ← parseExpr
        return $ List [Atom "quote", x]

parseString ∷ Parser LispVal
parseString = 
    do
        char '"'
        x <- many (noneOf "\"")
        char '"'
        return $ String x

parseExpr =   
        parseAtom
    <|> parseNumber
    <|> parseString 
    <|> parseNumber
    <|> parseQuoted
    <|> (do 
            char '('
            x <- try parseList <|> parseDottedList
            char ')'
            return x)
    

readExpr ∷ String → String
readExpr input = 
    case parse parseExpr "lisp" input of

        Left err → 
            "No match: " ++ show err

        Right val → 
            "Found value"


main ∷ IO ()
main = do 
         (expr:_) ← getArgs
         putStrLn (readExpr expr)
