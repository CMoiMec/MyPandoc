{-
-- EPITECH PROJECT, 2025
-- pandoc
-- File description:
-- Lib
-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}


module ParseFile where
import Control.Applicative (Alternative(..), (<|>))

-- refactoring to newtype
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- basic parsers with the new type
parseChar :: Char -> Parser Char
parseChar c = Parser $ \input -> case input of
    (x:xs) | x == c -> Just (c, xs)
    _               -> Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar chars = Parser $ \input -> case input of
    (x:xs) | x `elem` chars -> Just (x, xs)
    _                       -> Nothing

-- making Parser a Functor
instance Functor Parser where
    fmap f parser = Parser $ \input -> case runParser parser input of
        Just (result, rest) -> Just (f result, rest)
        Nothing            -> Nothing

-- making Parser an Applicative
instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    
    pf <*> pa = Parser $ \input -> case runParser pf input of
        Just (f, rest) -> runParser (fmap f pa) rest
        Nothing       -> Nothing

-- making Parser an Alternative
instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    
    p1 <|> p2 = Parser $ \input -> case runParser p1 input of
      Just result -> Just result
      Nothing    -> runParser p2 input

-- making Parser a Monad
instance Monad Parser where
    return = pure
    
    pa >>= f = Parser $ \input -> case runParser pa input of
        Just (a, rest) -> runParser (f a) rest
        Nothing       -> Nothing

-- using Alternative for parseOr
parseOr :: Parser a -> Parser a -> Parser a
parseOr = (<|>)

-- using Applicative for parseAnd
parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd pa pb = (,) <$> pa <*> pb

-- using Applicative for parseAndWith
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f pa pb = f <$> pa <*> pb

-- alternative implementations for parseMany and parseSome
parseMany :: Parser a -> Parser [a]
parseMany p = some p <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome p = (:) <$> p <*> parseMany p

-- rewriting parseTuple with Applicative
parseTuple :: Parser a -> Parser (a, a)
parseTuple p =
    parseChar '(' *> ((,) <$> p <* parseChar ',' <*> p) <* parseChar ')'