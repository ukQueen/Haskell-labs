module Main (main) where

import Lib
import Control.Exception (IOException, try)

main :: IO ()
main = do
    menu 

menu :: IO()
menu = do
    openParser
    menu


openParser :: IO()
openParser = do
    putStrLn "-----------------------------------------------------------"
    putStrLn "write path to .txt file (default \"files\\Operations.txt\")"
    inputF <- getLine
    let fileName = if null inputF then  "files\\Operations.txt" else inputF :: String
    openning <- try (readFile fileName) :: IO (Either IOException String)
    case openning of
        Left ex -> putStrLn $ "Failed to open file: " ++ show ex
        Right text -> do
            let linesOfText = lines text
            processLine linesOfText



processLine :: [String] -> IO()
processLine [] = putStrLn "\nNo more lines to process."
processLine (x:xs) = do
    putStrLn ""
    putStrLn $ "Processing: " ++ x
    case compareBinaries x of
        Just result -> putStrLn $ "Result: " ++ show result
        Nothing     -> putStrLn "Error: Unable to parse the input."
    processLine xs

