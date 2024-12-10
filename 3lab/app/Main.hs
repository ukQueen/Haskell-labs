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
 --   putStrLn "[1] - encoding text into an image"
 --   putStrLn "[2] - decoding text from the image"
    putStrLn "[1] - encrypting text"
    putStrLn "[2] - encrypting text with coordinates"
    putStrLn "[3] - decrypting text"
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
                    menuEncrypting
                    putStrLn "---------------------------------"
                    menu
                2 -> do
                    menuEncryptingCoordinates
                    putStrLn "---------------------------------"
                    menu
                3 -> do
                    menuDecrypting
                    putStrLn "---------------------------------"
                    menu
                0 -> putStrLn "Exiting..."
                _ -> do
                    putStrLn "Invalid choice. Try again."
                    putStrLn "---------------------------------"
                    menu


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


menuEncryptingCoordinates :: IO()
menuEncryptingCoordinates = do
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
            putStrLn $ "image size: " ++ (show width) ++ " x " ++ (show height)

            putStrLn "write path to .txt file (default \"files\\Alonzo Church's Biography.txt\")"
            inputF <- getLine
            let fileName = if null inputF then  "files\\Alonzo Church's Biography.txt" else inputF :: String
            openning <- try (readFile fileName) :: IO (Either IOException String)
            case openning of
                Left ex -> putStrLn $ "Failed to open file: " ++ show ex
                Right text -> do
                    let binaryEncodedText = binaryEncoding $ encoding key text
                    putStrLn $ "Lenght of text in bits: " ++ show  (length binaryEncodedText)
                    putStrLn "Write Key for encoding (default \"Haskell\"):"
                    input1 <- getLine
                    let key = if null input1 then "Haskell" else input1 :: String

                    putStrLn "Write count of bits (default 4):"
                    input2 <- getLine
                    let inp = if null input2 then 4 else read input2 :: Int
                    count <- check inp 

                    putStrLn "Coordinates for encrypting write coordinate x (default 0):"
                    inputX <- getLine
                    let coordX = if null inputX then 0 else read inputX :: Int
                    putStrLn "Coordinates for encrypting write coordinate y (default 0):"
                    inputY <- getLine
                    let coordY = if null inputY then 0 else read inputY :: Int

                    -- let message = 
                    --     if ((width - coordX) * (height - y) * 3 * count) == length binaryEncodedText

                    (x, y) <- coordinate (coordX, coordY, width, height, count, (length binaryEncodedText))

                    let encryptedText = encryptingCoordinates binaryImage binaryEncodedText count x y width height
                        img' = binaryStringToImage encryptedText width height
                        img'' = ImageRGB8 img'
                    putStrLn "text is encrypting..."
                    encryptedText `deepseq` putStrLn "text encrypted..."
                    putStrLn "image is saving..."
                    let filePath = "files/" ++ key ++ "_" ++ show count ++ ".bmp"
                    saveBmpImage filePath img''
                    putStrLn $ "Image saved to \"" ++ filePath ++ "\""


coordinate :: (Int, Int, Int, Int, Int, Int) -> IO (Int, Int)
coordinate (x, y, width, height, count, size) =
    if x < width && y < height 
        then 
            if ((width - x + 1) * (height - y + 1) * 3 * count) > size
                then 
                    return (x, y)
                else do
                    putStrLn "invailed coordinates, counts of the bits in text don't match. Try again"
                    putStrLn "Coordinates for encrypting write coordinate x (default 0):"
                    inputX <- getLine
                    let coordX = if null inputX then 0 else read inputX :: Int
                    putStrLn "Coordinates for encrypting write coordinate y (default 0):"
                    inputY <- getLine
                    let coordY = if null inputY then 0 else read inputY :: Int
                    coordinate (coordX, coordY, width, height, count, size)
        else do
            putStrLn "invalid coordinates. Try again"
            putStrLn "Coordinates for encrypting write coordinate x (default 0):"
            inputX <- getLine
            let coordX = if null inputX then 0 else read inputX :: Int
            putStrLn "Coordinates for encrypting write coordinate y (default 0):"
            inputY <- getLine
            let coordY = if null inputY then 0 else read inputY :: Int
            coordinate (coordX, coordY, width, height, count, size)



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