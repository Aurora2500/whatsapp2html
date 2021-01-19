module Parser where

import Control.Applicative
import Control.Monad
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

instance Semigroup a => Semigroup (Parser a) where
    p1 <> p2 = do
        r1 <- p1
        r2 <- p2
        return $ r1 <> r2

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

takePredP :: Int -> (Char -> Bool) -> Parser String
takePredP n pred = do
    parsed <- takeP n
    guard $ and $ map pred parsed
    return $ parsed

spanToP :: Int -> (Char -> Bool) -> Parser String
spanToP n pred =
    Parser $ \input ->
        let Just (rest, token) = runParser (spanP pred) input
            final = take n token
            rest' = drop (length final) input
         in Just (rest', final)

allP :: Parser String
allP = Parser $ \input -> Just ("", input)

choice :: Alternative f => [f a] -> f a
choice = foldl (<|>) empty

parseAny :: [String] -> Parser String
parseAny cs = choice $ map stringP cs
