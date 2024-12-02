module Main (main) where

import Lib
import Codec.Picture
import System.IO
import Control.DeepSeq (deepseq)
import Control.Exception (IOException, try)


main :: IO ()
main = do
    menu 

menu :: IO()
menu = do
    putStrLn "Choose the action:"
    putStrLn "[1] - encoding text into an image"
    putStrLn "[2] - decoding text from the image"
    putStrLn "[3] - encrypting text"
    putStrLn "[4] - decrypting text"
    putStrLn "[0] - Bye!"
    input <- getLine
    if null input  -- Проверка на пустой ввод
        then do
            putStrLn "You must choose a valid option. Please try again."
            menu
        else do
            let n = read input :: Int
            case n of    
                1 -> do 
                    menuEncoding
                    putStrLn "---------------------------------"
                    menu
                2 -> do 
                    menuDecoding
                    putStrLn "---------------------------------"
                    menu
                3 -> do
                    menuEncrypting
                    putStrLn "---------------------------------"
                    menu
                4 -> do
                    menuDecrypting
                    putStrLn "---------------------------------"
                    menu
                0 -> putStrLn "Exiting..."
                _ -> do
                    putStrLn "Invalid choice. Try again."
                    putStrLn "---------------------------------"
                    menu


menuEncoding:: IO()
menuEncoding = do
    putStrLn "write path to .txt file, that you want to encode (default \"files\\Alonzo Church's Biography.txt\")"
    input <- getLine
    let fileName = if null input then  "files\\Alonzo Church's Biography.txt" else input :: String
    openning <- try (readFile fileName) :: IO (Either IOException String)
    case openning of
        Left ex -> putStrLn $ "Failed to open file: " ++ show ex
        Right text -> do
            putStrLn "Write Key for encoding (default \"Haskell\"):"
            input1 <- getLine
            let key = if null input1 then "Haskell" else read input1 :: String

            putStrLn "Write width of image (default 30):"
            input2 <- getLine
            let width = if null input2 then 30 else read input2 :: Int
            
            putStrLn "Write height of image (default 30):"
            input3 <- getLine
            let height = if null input3 then 30 else read input3 :: Int
                binaryEncodedText = binaryEncoding $ encoding key text
                img = binaryStringToImage binaryEncodedText width height
                img' = ImageRGB8 img
                filePath = "files/" ++ key ++ ".bmp"
            saveBmpImage filePath img'
            putStrLn $ "Image saved to \"" ++ filePath ++ "\""


menuDecoding:: IO()
menuDecoding = do
    putStrLn "write path to the image, that you want to decode (default \"files\\Haskell.bmp\")"
    input <- getLine
    let path = if null input then  "files\\Haskell.bmp" else input :: String
        key = getKey path
    eimg <- readImage path
    case eimg of
        Left err -> putStrLn $ "Failed to open image: " ++ err
        Right dynImg -> do 
            let img = convertRGB8 dynImg   
                bStr = imageToBinary img
                binaryDecodedText = binaryDecoding bStr
                decodedText = decoding key binaryDecodedText
            putStrLn $ "write path to save decoded image (default \"files\\" ++ key ++ ".txt\")"
            input1 <- getLine
            let fileName = if null input1 then  "files\\" ++ key ++ ".txt" else input1 :: String
            file <- openFile fileName WriteMode
            hPutStr file decodedText
            hClose file
            putStrLn $ "Decoded image saved to " ++ fileName



check :: Int -> IO Int
check count  = 
    if count >= 0 && count <= 8 then do return count
    else do
      putStrLn ("Input error. Try again")
      input <- getLine
      let count' = read input :: Int
      check count'


menuEncrypting :: IO()
menuEncrypting = do
    putStrLn "write path to the image (default \"files\\Alonzo_Church.bmp\")"
    input <- getLine
    let path = if null input then  "files\\Alonzo_Church.bmp" else input :: String
        key = getKey path
    eimg <- readImage path
    case eimg of
        Left err -> putStrLn $ "Failed to open image: " ++ err
        Right dynImg -> do 
            let img = convertRGB8 dynImg   
                binaryImage = imageToBinary img
                height = imageHeight img
                width = imageWidth img

            putStrLn "write path to .txt file (default \"files\\Alonzo Church's Biography.txt\")"
            inputF <- getLine
            let fileName = if null inputF then  "files\\Alonzo Church's Biography.txt" else inputF :: String
            openning <- try (readFile fileName) :: IO (Either IOException String)
            case openning of
                Left ex -> putStrLn $ "Failed to open file: " ++ show ex
                Right text -> do
                    putStrLn "Write Key for encoding (default \"Haskell\"):"
                    input1 <- getLine
                    let key = if null input1 then "Haskell" else input1 :: String

                    putStrLn "Write count of bits (default 4):"
                    input2 <- getLine
                    let inp = if null input2 then 4 else read input2 :: Int
                    count <- check inp 
                    let binaryEncodedText = binaryEncoding $ encoding key text
                        encryptedText = encrypting binaryImage binaryEncodedText count
                        img' = binaryStringToImage encryptedText width height
                        img'' = ImageRGB8 img'
                    putStrLn "text is encrypting..."
                    encryptedText `deepseq` putStrLn "text encrypted..."
                    putStrLn "image is saving..."
                    let filePath = "files/" ++ key ++ "_" ++ show count ++ ".bmp"
                    saveBmpImage filePath img''
                    putStrLn $ "Image saved to \"" ++ filePath ++ "\""


menuDecrypting :: IO()
menuDecrypting = do
    putStrLn "write path to the image (default \"files\\Haskell_4.bmp\")"
    input <- getLine
    let path = if null input then  "files\\Haskell_4.bmp" else input :: String
        key = getKey path
    eimg <- readImage path
    case eimg of
        Left err -> putStrLn $ "Failed to open image: " ++ err
        Right dynImg -> do 
            let img = convertRGB8 dynImg   
                binaryImage = imageToBinary img
                (key, bits) = getKeyBits path
                bText = decrypting binaryImage bits
                binaryDecodedText = binaryDecoding bText
                decodedText = decoding key binaryDecodedText
            putStrLn $ "write path to save decrypted image (default \"files\\decrypdedImage.txt\")"
            input1 <- getLine
            let fileName = if null input1 then "files\\decrypdedImage.txt" else input1 :: String

            putStrLn "image is decrypting..."
            bText `deepseq` putStrLn "image decrypted..."
            putStrLn "text is saving..."
            file <- openFile fileName WriteMode
            hPutStr file decodedText
            hClose file
            putStrLn $ "Decrypted image saved to \"" ++ fileName ++ "\""