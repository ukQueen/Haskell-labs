{-# LANGUAGE OverloadedStrings #-}
module Lib (
    ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Control.Exception (try, IOException)
import System.Random (StdGen, mkStdGen)
import System.IO

-- Функция для конвертации Map String [(String, Int)] -> Map String [String]
convertMap :: Map String [(String, Int)] -> Map String [String]
convertMap = Map.map (map fst)

-- Функция processInput с исправленной сигнатурой
processInput :: Map String [String] -> String -> String
processInput vocabulary phrase =
    case Map.lookup phrase vocabulary of
        Just wordsList -> unwords wordsList
        Nothing        -> "Phrase not found in vocabulary"

-- Функция handleFile2 с конверсией
handleFile2 :: Map String [String] -> String -> IO ()
handleFile2 vocabulary content = do
    let linesContent = lines content
    mapM_ (\line -> putStrLn $ processInput vocabulary line) linesContent

-- Функция для генерации фразы
generatePhrase :: Map String [(String, Int)] -> String -> StdGen -> Int -> Int -> [String]
generatePhrase vocabulary phrase _ _ _ =
    case Map.lookup phrase vocabulary of
        Just wordList -> map fst wordList
        Nothing       -> ["Phrase not found"]

-- Функция handleGeneration
handleGeneration :: Map String [(String, Int)] -> Map String [(String, Int)] -> String -> Int -> IO ()
handleGeneration vocabulary1 vocabulary2 phrase count = do
    let currentMap = if count `mod` 2 == 0 then vocabulary1 else vocabulary2
        convertedMap = convertMap currentMap
        words = unwords $ generatePhrase currentMap phrase (mkStdGen 42) 10 20
    putStrLn words

-- Главная функция
main :: IO ()
main = do
    putStrLn "Enter the first vocabulary file:"
    fileName1 <- getLine
    result1 <- try (readFile fileName1) :: IO (Either IOException String)
    case result1 of
        Left ex -> putStrLn $ "Error reading file: " ++ show ex
        Right content1 -> do
            let vocabulary1 = parseVocabulary content1
            putStrLn "Enter the second vocabulary file:"
            fileName2 <- getLine
            result2 <- try (readFile fileName2) :: IO (Either IOException String)
            case result2 of
                Left ex -> putStrLn $ "Error reading file: " ++ show ex
                Right content2 -> do
                    let vocabulary2 = parseVocabulary content2
                    putStrLn "Enter a phrase:"
                    phrase <- getLine
                    let convertedVocabulary1 = convertMap vocabulary1
                        convertedVocabulary2 = convertMap vocabulary2
                    handleGeneration vocabulary1 vocabulary2 phrase 1
                    putStrLn $ "Processed phrase: " ++ processInput convertedVocabulary1 phrase

-- Вспомогательная функция для парсинга словаря из строки
parseVocabulary :: String -> Map String [(String, Int)]
parseVocabulary content =
    Map.fromListWith (++) $ map parseLine (lines content)
  where
    parseLine :: String -> (String, [(String, Int)])
    parseLine line =
        let (key:values) = words line
            parsedValues = map (\word -> (word, 1)) values
        in (key, parsedValues)
