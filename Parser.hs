module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char

import Lambda
import Code

newtype Parser a = Parser { parse :: String -> Maybe(a, String) }

-- 2.1. / 3.2.
-- Functor ---------------------------------------
instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f mp = Parser $ \str ->
        case parse mp str of
            Nothing -> Nothing
            Just(value, s) -> Just(f value, s) -- impachetez eu valoarea in Just() 

-- Applicative -----------------------------------
instance Applicative Parser where
    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    af <*> mp = 
        do 
            f <- af
            v <- mp
            return $ f v
    pure = return

-- Monad ----------------------------------------
instance Monad Parser where
    -- >>= :: Parser a -> (a -> Parser b) -> Parser b
    mp >>= f = Parser $ \str ->
        case parse mp str of
            Nothing -> Nothing
            Just(value, s) -> parse (f value) s -- apply, pentru ca trebuie sa returnez un Parser

    return s = Parser $ \str -> Just(s, str)

-- Alternative ---------------------------------
instance Alternative Parser where
    -- (<|>) :: Parser a -> Parser a -> Parser a
    empty = failParser
    p1 <|> p2 = Parser $ \str -> 
        case parse p1 str of 
            Nothing -> parse p2 str
            result -> result


-- implementarea parserelor de baza:
-- failParser - esueaza pt orice input
failParser :: Parser a
failParser = Parser $ \s -> Nothing

-- charParser - parseaza un caracter dat
charParser :: Char -> Parser Char
charParser c = Parser $ \str ->
    case str of
        [] -> Nothing
        (x:xs) -> if (x == c) then Just(x, xs) else Nothing 

-- plusParser - generatorul de parsere care se apeleaza de 1 sau mai multe ori
plusParser :: Parser a -> Parser [a]
plusParser mp = 
    do -- de ce asta e cu "do" si restul fara?? ----------------------------------------------------------------------------------------------------------
        x <- mp
        xs <- starParser mp
        return (x:xs)

-- starParser - generator de parsere care se apeleaza de 0 sau mai multe ori; foloseste Alternative
starParser :: Parser a -> Parser [a]
starParser mp = (plusParser mp) <|> (return [])

-- whitespaceParser
whitespaceParser :: Parser String 
whitespaceParser = starParser (charParser ' ')

-- implementarea parserelor efective:
alphaParser :: Parser Char
alphaParser = Parser $ \str ->
    case str of
        [] -> Nothing
        (x:xs) -> if (isAlpha x && isLower x) then Just(x, xs) else Nothing

variableParser :: Parser String
variableParser = plusParser alphaParser

atomParser :: Parser Lambda
atomParser = atomLambdaParser <|> atomVariableParser <|> macroParser
    where
        atomLambdaParser =
            do
                charParser '('
                x <- lambdaParser
                charParser ')'
                return x
        atomVariableParser = 
            do
                x <- variableParser
                return (Var x)
        
appParser :: Parser Lambda
appParser =
    do
        e1 <- atomParser
        e2 <- starParser
            (do
                whitespaceParser
                atomParser)
        return (foldl App e1 e2)

absParser :: Parser Lambda
absParser = 
    do
        charParser '\\'
        x <- variableParser
        charParser '.'
        e <- lambdaParser

        return (Abs x e)

lambdaParser :: Parser Lambda
lambdaParser = absParser <|> appParser

macroParser :: Parser Lambda
macroParser = 
    do
        m <- plusParser (Parser $ \str ->
                case str of
                    [] -> Nothing
                    (x:xs) -> if (isUpper x || isDigit x) then Just(x, xs) else Nothing)
        return (Macro m)

macroStringParser :: Parser String
macroStringParser = 
    do
        m <- plusParser (Parser $ \str ->
                case str of
                    [] -> Nothing
                    (x:xs) -> if (isUpper x || isDigit x) then Just(x, xs) else Nothing)
        return m

parseLambda :: String -> Lambda
parseLambda lambda_expr = 
    case parse lambdaParser lambda_expr of
        Nothing -> undefined
        Just(val, _) -> val

-- 3.3. ---------------------------------------
-- helpere pentru parsare
evalParser :: Parser Line
evalParser =
    do
        val <- lambdaParser
        return (Eval val)

bindingParser :: Parser Line
bindingParser = 
    do
        macro <- macroStringParser

        whitespaceParser
        charParser '='
        whitespaceParser
        
        expr <- lambdaParser
        return (Binding macro expr)

parseLine :: String -> Either String Line
parseLine lambda_expr = 
    case parse (bindingParser <|> evalParser) lambda_expr of
        Nothing -> Left "parser failed"
        Just (value, _) -> Right value