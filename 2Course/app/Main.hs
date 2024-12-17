module Main (main) where

import Lib

import System.IO
import Control.Exception (IOException, try)
import qualified Data.Map as Map
import System.Random (newStdGen)




main :: IO ()
main = menu 

menu :: IO()
menu = 
    putStrLn "Choose the action:" >>
    putStrLn "[1] - split the text into sentences" >>
    putStrLn "[2] - create vocabulary" >>
    putStrLn "[3] - generate sentence" >>
    putStrLn "[4] - talk models" >>
    putStrLn "[0] - exit" >>
    getLine >>= \input ->
        if null input  
            then 
                putStrLn "You must choose a valid option. Please try again." >>
                menu
            else 
                let n = read input :: Int
                in case n of    
                    1 -> 
                        splitText >>
                        putStrLn "---------------------------------" >>
                        menu
                    2 -> 
                        menuVocabulary >>
                        putStrLn "---------------------------------" >>
                        menu
                    3 -> 
                        generateText >>
                        putStrLn "---------------------------------" >>
                        menu
                    4 -> talkTwoModels >>
                        putStrLn "---------------------------------" >>
                        menu
                    0 -> putStrLn "Exiting..."
                    _ -> 
                        putStrLn "Invalid choice. Try again." >>
                        putStrLn "---------------------------------" >>
                        menu


splitText :: IO()
splitText = 
    putStrLn "write path to .txt file (default \"files\\The Garnet Bracelet.txt\")" >>
    getLine >>= \input ->
        let fileName = if null input then "files\\The Garnet Bracelet.txt" else input :: String
        in (try (readFile fileName) :: IO (Either IOException String)) >>= \openning ->
            case openning of
                Left ex -> putStrLn  ("Failed to open file: " ++ show ex) 
                Right text -> 

                    case getSentencens $ filter (/= '\n') text of 
                        Nothing -> putStrLn "No sentences found in the text."
                        Just sentences -> 
                            
                            putStrLn "write path to save splited text into senteces (default \"files\\Sentences.txt\")" >>
                            getLine >>= \input1 ->
                                let fileName = if null input1 then "files\\Sentences.txt" else input1 :: String
                                in openFile fileName WriteMode >>= \file ->
                                    let readySentencens = createOneString sentences
                                    in hPutStr file readySentencens >>
                                    hClose file


menuVocabulary :: IO()
menuVocabulary = 
    putStrLn "write path to .txt file (default \"files\\he Garnet Bracelet.txt\")" >>
    getLine >>= \input ->
        -- let fileName = if null input then "files\\Centences.txt" else input :: String
      --  let fileName = if null input then "files\\TestFile.txt" else input :: String
        let fileName = if null input then "files\\The Garnet Bracelet.txt" else input :: String
        in (try (readFile fileName) :: IO (Either IOException String)) >>= \openning ->
            case openning of
                Left ex -> putStrLn  ("Failed to open file: " ++ show ex) 
                Right text -> 
                    case createVocabulary text of 
                        Nothing -> putStrLn "No sentences found in the text."
                        Just vocabulary -> 
                            putStrLn "write path to save splited text into senteces (default \"files\\TestResult.txt\")" >>
                            getLine >>= \input1 ->
                                let fileName = if null input1 then "files\\TestResult.txt" else input1 :: String
                                in openFile fileName WriteMode >>= \file ->
                                    let readyVocabulary = writeVocabulary vocabulary
                                    in hPutStr file readyVocabulary >>
                                    hClose file
    

generateText :: IO()
generateText = 
    putStrLn "write path to .txt file (default \"files\\The Garnet Bracelet.txt\")" >>
    getLine >>= \input ->
        -- let fileName = if null input then "files\\Centences.txt" else input :: String
      --  let fileName = if null input then "files\\TestFile.txt" else input :: String
        let fileName = if null input then "files\\The Garnet Bracelet.txt" else input :: String
        in (try (readFile fileName) :: IO (Either IOException String)) >>= \openning ->
            case openning of
                Left ex -> putStrLn  ("Failed to open file: " ++ show ex) 
                Right text -> 
                    case createVocabulary text of 
                        Nothing -> putStrLn "No sentences found in the text."
                        Just vocabulary -> 
                            newStdGen >>= \gen -> 
                                putStrLn "write phrase" >>
                                getLine >>= \inputn ->
                                    let phrase = inputn :: String
                                    in processInput vocabulary phrase
                                    


talkTwoModels :: IO ()
talkTwoModels =
    putStrLn "write path to .txt file (default \"files\\The Garnet Bracelet.txt\")" >>
    getLine >>= \inputf1 ->
        let fileName1 = if null inputf1 then "files\\The Garnet Bracelet.txt" else inputf1
        in try (readFile fileName1) >>= handleFile1

handleFile1 :: Either IOException String -> IO ()
handleFile1 (Left ex) = putStrLn ("Failed to open file: " ++ show ex)
handleFile1 (Right text1) =
    case createVocabulary text1 of
        Nothing -> putStrLn "No sentences found in the text."
        Just vocabulary1 ->
            putStrLn "write path to .txt file (default \"files\\Yama.txt\")" >>
            getLine >>= \inputf2 ->
                let fileName2 = if null inputf2 then "files\\Yama.txt" else inputf2
                in try (readFile fileName2) >>= handleFile2 vocabulary1

handleFile2 :: Map.Map String [String] -> Either IOException String -> IO ()
handleFile2 _ (Left ex) = putStrLn ("Failed to open file: " ++ show ex)
handleFile2 vocabulary1 (Right text2) =
    case createVocabulary text2 of
        Nothing -> putStrLn "No sentences found in the text."
        Just vocabulary2 ->
            putStrLn "write count of messages:" >>
            getLine >>= \inputCount ->
                putStrLn "write phrase:" >>
                getLine >>= \phrase ->
                    let count = read inputCount :: Int
                    in if count `mod` 2 == 0
                        then handleGeneration vocabulary1 vocabulary2 phrase count
                        else handleGeneration vocabulary2 vocabulary1 phrase count

handleGeneration :: Map.Map String [String] -> Map.Map String [String] -> String -> Int -> IO ()
handleGeneration vocab1 vocab2 phrase count
    | Map.member phrase vocab1 =
        generatingMessages count phrase vocab1 vocab2
    | otherwise = putStrLn "Phrase not found in the vocabulary."


generatingMessages :: Int -> String -> Map.Map String [String] -> Map.Map String [String] -> IO ()
generatingMessages 0 _ _ _ = return ()
generatingMessages count phrase map1 map2 =
    newStdGen >>= \gen ->
        let botNumber = if count `mod` 2 == 0 then "1 bot" else "2 bot"
            currentMap = if count `mod` 2 == 0 then map1 else map2
            words =  generatePhrase currentMap phrase gen 10 20
        in case getLastValidWord words currentMap of
            Nothing -> do
                putStrLn $ botNumber ++ " : Can't find a valid phrase in the vocabulary."
                generatingMessages (count - 1) phrase map1 map2
            Just lastWord -> do
                putStrLn $ botNumber ++ " : "  ++ phrase ++ " " ++ unwords words
                generatingMessages (count - 1) lastWord map1 map2

getLastValidWord :: [String] -> Map.Map String [String] -> Maybe String
getLastValidWord [] _ = Nothing
getLastValidWord (x:xs) vocab
    | Map.member x vocab = Just x
    | otherwise = getLastValidWord xs vocab




processInput :: Map.Map String [String] -> String -> IO ()
processInput dict input =
    if Map.member input dict then
        newStdGen >>= \gen ->
        putStrLn $ input ++ " " ++ unwords (generatePhrase dict input gen 2 15)
    else
        putStrLn "Phrase not found in the vocabulary."



