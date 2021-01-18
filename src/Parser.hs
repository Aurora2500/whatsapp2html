module Parser where

import Control.Applicative
import Data.Char
import Data.Maybe

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (rest, a) <- p input
            Just (rest, f a)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    (Parser p1) >>= f =
        Parser $ \input -> do
            (input', a) <- p1 input
            runParser (f a) input'

charP :: Char -> Parser Char
charP c = Parser f
    where
        f (x:xs)
            | x == c = Just (xs, c)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
         in Just (rest, token)

spaceP :: Parser Char
spaceP = charP ' '

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

intP :: Parser Int
intP = fmap read $ notNull $ spanP isNumber

takeP :: Int -> Parser String
takeP x = Parser f
    where
        f input
             | length input < x = Nothing
             | otherwise = Just (drop x input, take x input)

allP :: Parser String
allP = Parser $ \input -> Just ("", input)
