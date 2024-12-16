module Lib
    ( compareBinaries
    ) where

import Data.Char (isLower, isDigit, digitToInt)
import Control.Applicative


newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok], a) }

satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (c:cs) | pr c = Just (cs,c)
    f _ = Nothing 

lower :: Parser Char Char
lower = satisfy isLower

char :: Char -> Parser Char Char
char x = satisfy (== x)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit


instance Functor (Parser tok) where
    --fmap :: (a -> b) -> Parser tok a -> Parser tok b
    fmap g (Parser p) = Parser f where
        f xs = case p xs of
            Nothing -> Nothing
           -- Just (_, empty) -> Nothing
            Just (cs, c) -> Just (cs, g c)

instance Applicative (Parser tok) where
    --pure :: a -> Parser tok a
    pure a = Parser $ \s -> Just (s, a)

    --(<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b 
    Parser p1 <*> Parser p2 = Parser f where
        f xs = case p1 xs of
            Nothing -> Nothing
            Just (xs', g) -> case p2 xs' of 
                Nothing -> Nothing
                Just (xs'', c) -> Just (xs'', g c)



instance Alternative (Parser tok) where
    -- empty :: Parser tok a
    empty = Parser $ \_ -> Nothing

    -- (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser p1 <|> Parser p2 = Parser f where
        f xs = case p1 xs of
            Nothing -> p2 xs 
            z -> z




isOperator :: Parser Char Char
isOperator = satisfy (`elem` "<=>")

operator :: Parser Char Char
operator = many (satisfy (not . (`elem` "<=>"))) *> isOperator


isBinaryDigit :: Parser Char Char
isBinaryDigit = satisfy (`elem` "01 ")

binaryString :: Parser Char String
binaryString = pure (:) <*> isBinaryDigit <*> binaryString <|> pure []

-- binaryToInt :: String -> Int
-- binaryToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

removeSpaces :: Parser Char String
removeSpaces = pure (filter (/= ' ')) <*> binaryString

parseNotEmpty :: Parser Char String ->  Parser Char String 
parseNotEmpty (Parser p) = Parser f where
    f xs = case p xs of 
        Nothing -> Nothing
        Just (_, []) -> Nothing
        Just (cs, c) -> Just (cs, c)

-- число из двоичной строки
fromBinary ::  [Char] -> Int 
fromBinary str 
    |null str = 0 
    |otherwise = num + fromBinary str'
        where 
            num = if head str == '1' then 2 ^ (length str - 1) else 0 
            str' = tail str

parseToInt:: Parser Char Int
parseToInt = pure fromBinary <*> (parseNotEmpty removeSpaces)


less :: Parser Char Bool
less = pure (<) <*> parseToInt  <* char '<' <*>   parseToInt

more :: Parser Char Bool
more = pure (>) <*> parseToInt  <* char '>' <*>   parseToInt

equal :: Parser Char Bool
equal = pure (==) <*> parseToInt  <* char '=' <*>   parseToInt


compareBinaries :: String -> Maybe Bool
compareBinaries input = case runParser operator input of
    Just (_, '<') -> case runParser less input of
        Just (_, result) -> Just result
        Nothing -> Nothing
    Just (_, '>') -> case runParser more input of
        Just (_, result) -> Just result
        Nothing -> Nothing
    Just (_, '=') -> case runParser equal input of
        Just (_, result) -> Just result
        Nothing -> Nothing
    Nothing -> Nothing


