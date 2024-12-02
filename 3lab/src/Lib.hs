module Lib
    ( encoding, decoding, binaryEncoding, binaryDecoding, imageToBinary, binaryStringToImage,  getKey, getKeyBits, encrypting, decrypting
    ) where

import Numeric (showIntAtBase)
import Data.Char (ord, intToDigit)
import Data.Word (Word8)
import Codec.Picture
import Data.Char (digitToInt)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector (Vector)
import Data.Char (chr)
import Control.Monad.ST





-- убирает дубликаты в тексте
removeDublicates :: String -> String  
removeDublicates = removeDublicates' []
  where
    removeDublicates' :: String -> String -> String
    removeDublicates' _ [] = []
    removeDublicates' seen (x:xs)
      | x `elem ` seen = removeDublicates' seen xs
      | otherwise = x : removeDublicates' (x:seen) xs

-- конвертирует символы из одного алфавита в другой
convertText :: String -> String -> String -> String
convertText [] _ _ = []
convertText (c:cs) alphabet newAlphabet =
    let converted = case lookup c (zip alphabet newAlphabet) of
                      Just x -> x
                      Nothing -> c
    in converted : convertText cs alphabet newAlphabet


--создает алфавит на основе ключа (кодировка кодовым словом)
createAlphabet :: String -> String 
createAlphabet [] = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
createAlphabet str = 
    let newStr = str ++ " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
    in  removeDublicates newStr


-- кодирует текст с помощью кодировки кодовым словом
encoding :: String -> String -> String
encoding key text =
    let alphabet = createAlphabet [] 
        newAlphabet = createAlphabet key
    in convertText text alphabet newAlphabet


-- декодирует текст с помощью кодировки кодовым словом
decoding :: String -> String -> String
decoding key text = 
    let alphabet = createAlphabet key
        defaultAlphabet = createAlphabet []
    in convertText text alphabet defaultAlphabet 

-- число в двоичную строку
toBinary :: Int -> Vector Char 
toBinary n = V.fromList (replicate (8 - length bin) '0') V.++ V.fromList bin
    where bin = showIntAtBase 2 intToDigit n ""

-- число из двоичной строки
fromBinary :: Vector Char -> Int 
fromBinary str 
    | V.null str = 0 
    |otherwise = num + fromBinary str'
        where 
            num = if V.head str == '1' then 2 ^ (length str - 1) else 0 
            str' = V.tail str
          
-- преобразовывает символ в двоичную строку
charToBinary :: Char -> Vector Char
charToBinary c = toBinary $ ord c

-- получает символ из двоичной строки
charFromBinary :: Vector Char -> Char
charFromBinary c =  chr $ fromBinary c 

-- текст в двоичный код
binaryEncoding :: String -> Vector Char
binaryEncoding [] = V.empty
binaryEncoding (c:cs) = charToBinary c V.++ binaryEncoding cs


-- текст из двоичного кода
binaryDecoding :: Vector Char -> String
binaryDecoding str
    | V.null str = []
    | V.length str < 8 = []
    | otherwise = charFromBinary (V.take 8 str) : binaryDecoding (V.drop 8 str)


-- пиксель в двоичный код
pixelToBinary :: PixelRGB8 -> Vector Char
pixelToBinary (PixelRGB8 r g b) = toBinary (fromIntegral r) V.++ toBinary (fromIntegral g) V.++ toBinary (fromIntegral b) 


-- изображение в двоичный код
imageToBinary :: Image PixelRGB8 -> Vector Char
imageToBinary img = V.concat [pixelToBinary (pixelAt img x y) | y <- [0..height-1], x <- [0..width-1]]
    where 
        width = imageWidth img
        height = imageHeight img
    

-- дополняет двоичную строку нулями до вводимого размера
takeCurrentSize :: Vector Char -> Int -> Vector Char
takeCurrentSize str size 
    | length str < size = str V.++ V.replicate (size - V.length str) '0'
    | otherwise = V.take size str


-- создает два списка на основе переданого: n первых элемента, список без первых n элементов 
takeAndDrop :: Int -> Vector Char ->(Vector Char, Vector Char)
takeAndDrop n arr = (V.take n arr, V.drop n arr)


-- двоичную строку преобразует в цвет (8 битов в r, g или b )
binaryStringToColor :: Vector Char -> Word8
binaryStringToColor str = fromIntegral $ foldl (\acc x -> acc * 2 + digitToInt x) 0 str


-- двочиную строку превращает в изображение
binaryStringToImage :: Vector Char -> Int -> Int -> Image PixelRGB8
binaryStringToImage str width height = 
    let size = height * width * 3 * 8
        newStr = takeCurrentSize str size
        pixelRenderer x y =
            let index = (y * width + x) * 3 * 8
                r = binaryStringToColor (V.take 8 (V.drop index newStr))
                g = binaryStringToColor (V.take 8 (V.drop (index + 8) newStr))
                b = binaryStringToColor (V.take 8 (V.drop (index + 16) newStr))

            in PixelRGB8 r g b
    in generateImage pixelRenderer width height

-- достает название файла из пути
getKey :: String -> String
getKey path = 
    takeWhile (/= '.') (reverse (takeWhile (\c -> c /= '\\' && c /= '/') (reverse path)))

-- encrypting :: Vector Char -> Vector Char -> Int -> Vector Char
-- encrypting img str count
--     | V.null img = V.empty
--     | otherwise =  
--         let currentStr = takeCurrentSize str (V.length img `div` 8 * count)
--             img' = V.drop 8 img
--             currentStr' = V.drop count currentStr
--             prefix = V.take (8 - count) img
--             suffix = V.take count currentStr
--             rest = encrypting img' currentStr' count
--         -- in V.take (8 - count) img V.++ V.take count currentStr V.++ encrypting img' currentStr' count 
--         in V.concat[prefix, suffix, rest]

-- encrypting :: Vector Char -> Vector Char -> Int -> Vector Char
-- encrypting img str count = runST $ do
--     let indx = 0
--         len = length img
--         str' = takeCurrentSize str (len `div` 8 * count)
--     result <- MV.new len 
--     let go indx
--             | indx * 8 >= len  = return ()
--             | otherwise = do
--                 let elem0 = if count == 8 then str' V.! (indx * count)  else img V.! (indx * 8)
--                     elem1 = if count >= 7 then str' V.! (indx * count + 1 - (8 - count)) else img V.! (indx * 8 + 1)
--                     elem2 = if count >= 6 then str' V.! (indx * count + 2 - (8 - count)) else img V.! (indx * 8 + 2)
--                     elem3 = if count >= 5 then str' V.! (indx * count + 3 - (8 - count)) else img V.! (indx * 8 + 3)
--                     elem4 = if count >= 4 then str' V.! (indx * count + 4 - (8 - count)) else img V.! (indx * 8 + 4)
--                     elem5 = if count >= 3 then str' V.! (indx * count + 5 - (8 - count)) else img V.! (indx * 8 + 5)
--                     elem6 = if count >= 2 then str' V.! (indx * count + 6 - (8 - count)) else img V.! (indx * 8 + 6)
--                     elem7 = if count >= 1 then str' V.! (indx * count + 7 - (8 - count)) else img V.! (indx * 8 + 7)
--                 MV.write result (indx * 8) elem0
--                 MV.write result (indx * 8 + 1) elem1
--                 MV.write result (indx * 8 + 2) elem2
--                 MV.write result (indx * 8 + 3) elem3
--                 MV.write result (indx * 8 + 4) elem4
--                 MV.write result (indx * 8 + 5) elem5
--                 MV.write result (indx * 8 + 6) elem6
--                 MV.write result (indx * 8 + 7) elem7
--                 go (indx + 1)
--     go 0
--     V.freeze result



encrypting :: Vector Char -> Vector Char -> Int -> Vector Char
encrypting img str count = runST $ do
    let indx = 0
        len = length img
        str' = takeCurrentSize str (len `div` 8 * count)
    result <- MV.new len 
    let go indx
            | indx * 8 >= len  = return ()
            | otherwise = do
                V.forM_ (V.fromList [0..7]) $ \i -> do
                    let elem = if count >= (8 - i)
                               then str' V.! (indx * count + i - (8 - count))
                               else img V.! (indx * 8 + i)
                    MV.write result (indx * 8 + i) elem
                go (indx + 1)
    go 0
    V.freeze result


getKeyBits :: String -> (String, Int)
getKeyBits path = 
    let fileName = takeWhile (/= '.') (reverse (takeWhile (\c -> c /= '\\' && c /= '/') (reverse path)))
        key = takeWhile (/= '_') fileName
        bits = takeWhile (/= '.') (filter  (/= '_') (dropWhile (/= '_') fileName))
        bits' = read bits::Int
    in (key, bits')


decrypting :: Vector Char -> Int -> Vector Char
decrypting img count = runST $ do
    let indx = 0
        len = length img
    result <- MV.new (len `div` 8 * count) 
    let go indx
            | indx * 8 >= len  = return ()
            | otherwise = do
                V.forM_ (V.fromList [0..7]) $ \i -> do
                    let elem = if count >= (8 - i)
                               then img V.! (indx * 8 + i )
                               else '\0'
                    if elem /= '\0' 
                        then MV.write result (indx * count + i - (8 - count)) elem
                        else return ()
                go (indx + 1)
    go 0
    V.freeze result
