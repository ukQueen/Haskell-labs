{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( getSentencens, createOneString, createVocabulary, writeVocabulary, generatePhrase
    ) where

import Data.Char (isLower, isDigit, digitToInt)
import Control.Applicative
import qualified Data.Map as Map
import Data.List (nub)
import UnescapingPrint (ushow)
import System.Random (randomR, StdGen)


newtype Parser tok a = Parser { runParser :: [tok] -> Maybe ([tok], a) }


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


satisfy :: (tok -> Bool) -> Parser tok tok
satisfy pr = Parser f where
    f (c:cs) | pr c = Just (cs,c)
    f _ = Nothing 


isPunctuation :: Parser Char Char
isPunctuation = satisfy (`elem` ".!?;:()")

isNotPunctuation :: Parser Char Char
isNotPunctuation = satisfy (not . (`elem` ".!?;:()"))

text :: Parser Char String
text = pure (:) <*> isNotPunctuation  <*> text <|> pure []


splitByPunctuation :: Parser Char [String]
splitByPunctuation = some (text <* isPunctuation)


deleteFirstSpace :: String -> String
deleteFirstSpace [] = []
deleteFirstSpace (' ':xs) = deleteFirstSpace xs
deleteFirstSpace x = x

readyText :: Parser Char [String]
readyText = fmap (map deleteFirstSpace) splitByPunctuation


getSentencens :: String -> Maybe [String]
getSentencens input = case runParser readyText input of
    Just (_, result) -> 
        let readyResult = editSentenses result
        in Just readyResult
    Nothing -> Nothing

editSentenses :: [String] -> [String]
editSentenses sentences = func sentences where
    func [] = []
    func (x:xs) = filter(not. (`elem` "\"#$%&'*+,-/0123456789<=>@[\\]^_`{|}\n\r\t")) x : func xs

createOneString :: [String] -> String
createOneString sentences = func sentences where
    func [] = ""
    func (x:xs) = x ++ "\n" ++ func xs
            

emptyMap :: Map.Map String [String]
emptyMap = Map.empty

addKey :: String -> Map.Map String [String] -> Map.Map String [String]
addKey key map = case Map.lookup key map of
    Just _ -> map
    Nothing -> Map.insert key [] map

addKeys :: [String] -> Map.Map String [String] -> Map.Map String [String]
addKeys [] map = map
addKeys (x:xs) map = addKeys xs (addKey x map)

addItem :: String -> [String] -> Map.Map String [String] -> Map.Map String [String]
addItem key values map = 
    case Map.lookup key map of
        Just existValues -> add values where
            add [] = Map.insert key values map
            add (x:xs) = 
                if x `elem` existValues then 
                    add xs
                else 
                    Map.insert key (x:existValues) map
        Nothing -> Map.insert key values map

addItems :: [String] -> [String] -> Map.Map String [String] -> Map.Map String [String]
addItems [] _ map = map
addItems (x:xs) values map = 
    case Map.lookup x map of
        Just existValues -> addItems xs values (addItem x values map)
        Nothing -> addItems xs values (addItem x values map)


-- addValue :: String -> String -> Map.Map String [String] -> Map.Map String [String]
-- addValue key value map = 
--     (case Map.lookup key map of
--         Just values -> 
--             if value `elem` values then  
--                 Map.insert key values map
--             else 
--                 Map.insert key (value:values) map
--         Nothing -> Map.insert key [value] map)

addValue :: String -> String -> Map.Map String [String] -> Map.Map String [String]
addValue key value map = 
    (case Map.lookup key map of
        Just values -> 
            Map.insert key (value:values) map
        Nothing -> Map.insert key [value] map)

concatMaps :: Map.Map String [String] -> Map.Map String [String] -> Map.Map String [String]
concatMaps map1 map2 =
    let keys = Map.keys map1 ++ Map.keys map2
    in func keys emptyMap where
        func [] map = map
        func (x:xs) map = 
            case (Map.lookup x map1, Map.lookup x map2) of
                (Just values1, Just values2) -> 
                    let combinedValues =  (values1 ++ values2)
                    in func xs (Map.insert x combinedValues map)
                (Just values1, Nothing) -> 
                    func xs (Map.insert x values1 map)
                (Nothing, Just values2) -> 
                    func xs (Map.insert x values2 map)
                (Nothing, Nothing) -> 
                    func xs map
 

exampleMap1 :: Map.Map String [String]
exampleMap1 = Map.fromList [
    ("fruit", ["apple", "banana"]),
    ("vegetable", ["carrot", "lettuce"])
    ]
exampleMap2 :: Map.Map String [String]
exampleMap2 = Map.fromList [
    ("fruit", ["apple", "banana"]),
    ("vegetable", ["carrot", "lettuce"])
    ]

nGramm :: String -> Int -> Maybe (Map.Map String [String])
nGramm text n = 
    case getSentencens text of -- Maybe [String]
        Nothing -> Nothing
        Just sentences -> Just (func sentences emptyMap) where 
            func [] map = map
            func (x:xs) map =  -- предложения
                let words = splitWords x
                in func xs (func' words map)
            func' [] map = map
            func' (x':xs') map= -- слова
                let keys = getKeys n (x':xs')
                    values = reverse $ getValues n (x':xs') 
                    updatedMap = if n == 1 
                             then addKeys keys map
                             else let keyValuePairs = zip keys values
                                  in foldl (\acc (key, value) -> addValue key value acc) map keyValuePairs
               in func' xs' updatedMap
            
                
getKeys :: Int -> [String] -> [String]
getKeys n words = func n words [] where
    func _ [] result = result
    func n words result 
        | n <= 0 = result
        | n == 1 = if length result > 0 then result else take 1 words
        | length words < n = result
        | otherwise =
            let value = concatWords (take (n - 1) words)
            in func (n - 1) words (value : result)

getValues :: Int -> [String] -> [String]
getValues n words = func n words [] where
    func _ [] result = result
    func n (x:xs) result 
        | n <= 1 = result
        | length (x:xs) < n  = result
        | otherwise =
            let value = concatWords (take (n - 1) xs)
            in func (n - 1) xs (value : result)


splitWords :: String -> [String]
splitWords text = words text

concatWords :: [String] -> String
concatWords [] = []
concatWords (x:xs) = 
    if length xs > 0 then
        x ++ " " ++ concatWords xs
    else x ++ concatWords xs


createVocabulary :: String -> Maybe (Map.Map String [String])
createVocabulary text = 
    case (nGramm text 1, nGramm text 2, nGramm text 3) of
        (Just map1, Just map2, Just map3) -> 
            let combinedMap = concatMaps map1 map2
            in Just (concatMaps combinedMap map3)
        _ -> Nothing


writeVocabulary :: Map.Map String [String] -> String
writeVocabulary map = func (Map.toList map) where
    func [] = ""
    func ((key, value):xs') = key ++ " : "  ++ ushow (createPairs value) ++ "\n" ++ func xs'


createPairs :: [String] -> [(String, Int)]  
createPairs [] = []
createPairs words = 
    let uniqueWords = nub words
    in func uniqueWords words where
        func [] _ = []
        func (x:xs) words = 
            let count = length $ filter (== x) words
            in (x, count) : func xs words


generatePhrase :: Map.Map String [String] -> String -> StdGen -> Int -> Int -> [String]
generatePhrase dict start initGenState a b=
    let (len, initGenState') = randomR (a,b :: Int) initGenState
    in reverse $ gp start [] len initGenState'
    where
        gp :: String -> [String] -> Int -> StdGen -> [String]
        gp key acc n genState 
            | n <= 0 = acc
            | otherwise =
                case Map.lookup key dict of
                    Nothing -> acc
                    Just [] -> acc
                    Just vals ->
                        let (i, newGenState) = randomR (0, length vals - 1) genState
                            next = vals !! i
                        in
                            gp next (next:acc) (n - length (words next)) newGenState

