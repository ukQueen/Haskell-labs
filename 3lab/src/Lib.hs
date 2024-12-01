module Lib
    ( encoding, binaryEncoding, createAlphabet, removeDublicates, imageToBinary, binaryStringToImage, takeCurrentSize
    ) where

import Numeric (showIntAtBase, readHex)
import Data.Char (ord, intToDigit)
import Data.Word (Word8)
import Codec.Picture
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Char (digitToInt)

-- убирает дубликаты в тексте
removeDublicates :: String -> String  
removeDublicates = removeDublicates' []
  where
    removeDublicates' :: String -> String -> String
    removeDublicates' _ [] = []
    removeDublicates' seen (x:xs)
      | x `elem ` seen = removeDublicates' seen xs
      | otherwise = x : removeDublicates' (x:seen) xs

--функция которая используется в кодировании символов (сопоставляет символ из обыного алфавита в новый) 
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

-- число в двоичную строку
toBinary :: Int -> String 
toBinary n = replicate (8 - length bin) '0' ++ bin
    where bin = showIntAtBase 2 intToDigit n ""


-- код из ASCII символа в двоичную строку
charToBinary :: Char -> String
charToBinary c = toBinary (ord c)

-- текст в двоичный код
binaryEncoding :: String -> String
binaryEncoding [] = []
binaryEncoding (c:cs) = charToBinary c ++ binaryEncoding cs

-- пиксель в двоичный код
pixelToBinary :: PixelRGB8 -> String
pixelToBinary (PixelRGB8 r g b) = toBinary (fromIntegral r) ++ toBinary (fromIntegral g) ++ toBinary (fromIntegral b) 

-- изображение в двоичный код
imageToBinary :: Image PixelRGB8 -> String
imageToBinary img = concat [pixelToBinary (pixelAt img x y) | y <- [0..height-1], x <- [0..width-1]]
    where 
        width = imageWidth img
        height = imageHeight img
    
-- дополняет двоичную строку нулями до вводимого размера
takeCurrentSize :: String -> Int -> String
takeCurrentSize str size 
    | length str < size = str ++ replicate (size - length str) '0'
    | otherwise = take size str


-- создает два списка на основе переданого: n первых элемента, список без первых n элементов 
takeAndDrop :: Int -> [a] ->([a], [a])
takeAndDrop n arr = (take n arr, drop n arr)

-- двоичную строку преобразует в цвет (8 битов в r, g или b )
binaryStringToColor :: String -> Word8
binaryStringToColor str = fromIntegral $ foldl (\acc x -> acc * 2 + digitToInt x) 0 str

-- делит на куски по n элементов массив arr 
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n arr = take n arr : chunksOf n (drop n arr)

-- двочиную строку превращает в изображение
-- binaryStringToImage :: String -> Int -> Int -> Image PixelRGB8
-- binaryStringToImage str width height = 
--     let size = height * width * 3 * 8
--         newStr = takeCurrentSize str size
--         pixelRenderer x y =
--             let index = (y * width + x) * 3 * 8
--                 r = binaryStringToColor (take 8 (drop index newStr))
--                 g = binaryStringToColor (take 8 (drop (index + 8) newStr))
--                 b = binaryStringToColor (take 8 (drop (index + 16) newStr))

--             in PixelRGB8 r g b
--     in generateImage pixelRenderer width height

binaryStringToImage :: String -> Int -> Int -> Image PixelRGB8
binaryStringToImage str width height =
    let size = height * width * 3 * 8
        newStr = takeCurrentSize str size
        precomputed = map binaryStringToColor (chunksOf 8 newStr)
        pixelRenderer x y =
            let index = (y * width + x) * 3
                r = precomputed !! index
                g = precomputed !! (index + 1)
                b = precomputed !! (index + 2)
            in PixelRGB8 r g b
    in generateImage pixelRenderer width height