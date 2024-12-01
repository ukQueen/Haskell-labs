module Main (main) where

import Lib
import Codec.Picture
import Codec.Picture.Bitmap
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO
import Control.DeepSeq (deepseq)



main :: IO ()
main = do
    let text = "aHello World!"
        key = "girl2"
        alphabet = createAlphabet []
        newAlphabet = createAlphabet "A"
        encodedText = encoding key text 
        binaryEncodedText = binaryEncoding  encodedText 
    putStrLn alphabet
    putStrLn newAlphabet
 --   putStrLn text
--    putStrLn encodedText
 --   putStrLn binaryEncodedText
    --putStrLn $ takeCurrentSize binaryEncodedText 100 

    --let img = binaryStringToImage binaryEncodedText 5 5
    --savePngImage ("files/" ++ key ++ ".bmp") (ImageRGB8 img)

    --content <- readFile "files/Alonzo Church's Biography.txt"
    --let binaryContext = binaryEncoding $ encoding key content
    --let img = binaryStringToImage binaryEncodedText 500 500
    --savePngImage ("files/" ++ key ++ ".bmp") (ImageRGB8 img)


    -- putStrLn "Читаем изображение..."
    -- eimg <- readImage "files\\Alonzo_Church.bmp"
    -- case eimg of
    --     Left err -> putStrLn ("Ошибка чтения изображения: " ++ err)
    --     Right dynImg -> do 
    --         putStrLn "Изображение прочитано, конвертируем в RGB8..."
    --         let img = convertRGB8 dynImg   
    --             height = imageHeight img
    --             width = imageWidth img
    --             --putStrLn $ imageToBinary img
    --         putStrLn "Изображение конвертировано. Генерируем бинарные данные..."
    --         -- let newBinary = imageToBinary img
    --         content <- readFile "files/Alonzo Church's Biography.txt"
    --         let binaryContext = binaryEncoding $ encoding key content
    --         putStrLn $ "Длина бинарных данных: " ++ show (length binaryContext)
    --    --     putStrLn $ takeCurrentSize binaryContext 50 

    --         putStrLn "Создаем новое изображение..."
    --         let img' = binaryStringToImage binaryContext width height
    --             dynImg' = ImageRGB8  img'
    --         putStrLn "Сохраняем изображение..."
    --         let filePath = "files/" ++ key ++ ".bmp"
    --         saveImage filePath img'
    --         putStrLn "BMP изображение успешно сохранено!"

    putStrLn "Читаем изображение..."
    eimg <- readImage "files\\Alonzo_Church.bmp"
    case eimg of
        Left err -> putStrLn ("Ошибка чтения изображения: " ++ err)
        Right dynImg -> do 
            putStrLn "Изображение прочитано, конвертируем в RGB8..."
            let img = convertRGB8 dynImg   
                height = imageHeight img
                width = imageWidth img
                --putStrLn $ imageToBinary img
            putStrLn "Изображение конвертировано. Генерируем бинарные данные..."
            let newBinary = imageToBinary img
            putStrLn $ "Длина бинарных данных: " ++ show (length newBinary)
            putStrLn "Создаем новое изображение..."
            let img' = binaryStringToImage newBinary width height
            img' `deepseq` putStrLn "Изображение полностью сгенерировано!"
            let dynImg' = ImageRGB8 img'
            putStrLn "Сохраняем изображение..."
            let filePath = "files/" ++ key ++ ".bmp"
            --saveImage filePath img'
            --savePngImage filePath dynImg'

            putStrLn $ "Пиксель (0, 0): " ++ show (pixelAt img' 0 0)
            putStrLn $ "Пиксель (width-1, height-1): " ++ show (pixelAt img' (width-1) (height-1))

            -- let img'' = generateImage (\_ _ -> PixelRGB8 255 0 0) 500 500
            -- -- Сохраняем его как BMP
            saveBmpImage filePath dynImg'
            putStrLn "BMP изображение успешно сохранено!"


saveImage :: FilePath -> Image PixelRGB8 -> IO ()
saveImage filePath img = do
    putStrLn "Сохраняем изображение как PNG..."
    savePngImage filePath (ImageRGB8 img)
    putStrLn "Изображение успешно сохранено!"