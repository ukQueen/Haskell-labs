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


--import Data.List (elemIndex)


--removeDublicates :: String -> String
--removeDublicates [] = []
--removeDublicates (x:xs) 
--  | x `elem ` xs = removeDublicates xs
--  | otherwise = x : removeDublicates xs

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

--convertText :: String -> String -> String -> String
--convertText [] _ _ = []
--convertText (c:cs) alphabet newAlphabet =
--    let charIndex = elemIndex c alphabet
--    in case charIndex of
--        Just i -> (newAlphabet !! i) : convertText cs alphabet newAlphabet
--        Nothing -> c: convertText cs alphabet newAlphabet


--создает алфавит на основе ключа (кодировка кодовым словом)
createAlphabet :: String -> String 
--createAlphabet [] = " !“#$%&‘()*+,–./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}" -- ASCII 32 - 125
createAlphabet [] = " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
createAlphabet str = 
--    let newStr = str ++ " !“#$%&‘()*+,–./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}"
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
toBinary n = showIntAtBase 2 intToDigit n ""
-- toBinary :: Int -> B.ByteString 
-- toBinary n = B.pack . map (fromIntegral . digitToInt) . showIntAtBase 2 intToDigit n $ ""

-- код из ASCII символа в двоичную строку
charToBinary :: Char -> String
charToBinary c = toBinary (ord c)
-- charToBinary :: Char -> B.ByteString
-- charToBinary c = toBinary (ord c)

-- текст в двоичный код
binaryEncoding :: String -> String
binaryEncoding [] = []
binaryEncoding (c:cs) = charToBinary c ++ binaryEncoding cs
-- binaryEncoding :: String -> B.ByteString
-- binaryEncoding [] = B.empty
-- binaryEncoding (c:cs) = B.append (charToBinary c) (binaryEncoding cs)



-- пиксель в двоичный код
pixelToBinary :: PixelRGB8 -> String
pixelToBinary (PixelRGB8 r g b) = toBinary (fromIntegral r) ++ toBinary (fromIntegral g) ++ toBinary (fromIntegral b) 
--pixelToBinary (PixelRGB8 r g b) = append (append (toBinary (fromIntegral r)) (toBinary (fromIntegral g))) (toBinary (fromIntegral b))

-- pixelToBinary :: PixelRGB8 -> B.ByteString
-- pixelToBinary (PixelRGB8 r g b) = B.append (B.append (toBinary (fromIntegral r)) (toBinary (fromIntegral g))) (toBinary (fromIntegral b))


-- изображение в двоичный код
imageToBinary :: Image PixelRGB8 -> String
imageToBinary img = concat [pixelToBinary (pixelAt img x y) | y <- [0..height-1], x <- [0..width-1]]
    where 
        width = imageWidth img
        height = imageHeight img
    
-- дополняет двоичную строку нулями до вводимого размера
takeCurrentSize :: String -> Int -> String
takeCurrentSize str size 
    | length str < size = take size (str ++ repeat '0')
    | otherwise = take size str

-- takeCurrentSize :: B.ByteString -> Int -> B.ByteString
-- takeCurrentSize str size 
--     | B.length str < size = B.append str (B.replicate (size - B.length str) 48)
--     | otherwise = B.take size str

-- создает два списка на основе переданого: n первых элемента, список без первых n элементов 
takeAndDrop :: Int -> [a] ->([a], [a])
takeAndDrop n arr = (take n arr, drop n arr)

-- двоичную строку преобразует в цвет (8 битов в r, g или b )
binaryStringToColor :: String -> Word8
-- binaryStringToColor str = fromIntegral $ fst $ head $ readHex ("0x" ++ str)
binaryStringToColor str = fromIntegral $ foldl (\acc x -> acc * 2 + digitToInt x) 0 str
-- binaryStringToColor :: B.ByteString -> Word8
-- binaryStringToColor = B.foldl' (\acc x -> acc * 2 + fromIntegral x) 0




-- двоичный код в изображение
--binaryStringToImage :: String -> Int -> Int -> Image PixelRGB8
--binaryStringToImage str width height =
--    let 
--        size = height * width * 3 * 8
--        newStr = takeCurrentSize str size
--        pixelRenderer x y =
--            let (forR, withoutR) = takeAndDrop 8 newStr
--                (forG, withoutRG) = takeAndDrop 8 withoutR
--                (forB, withoutRGB) = takeAndDrop 8 withoutRG
--                r = binaryStringToColor forR
--                g = binaryStringToColor forG
--                b = binaryStringToColor forB
--            in PixelRGB8 r g b
--    in generateImage pixelRenderer width height
        
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


binaryStringToImage :: String -> Int -> Int -> Image PixelRGB8
binaryStringToImage str width height =
    let 
        size = height * width * 3 * 8
        newStr = takeCurrentSize str size
        pixelData = chunksOf (3 * 8) newStr -- делим строку на куски по 24 бита (по 8 бит на r, g, b)
        pixelRenderer x y =
            let index = y * width + x
                pixel = pixelData !! index
                (rBits, rest1) = splitAt 8 pixel
                (gBits, bBits) = splitAt 8 rest1
                r = binaryStringToColor rBits
                g = binaryStringToColor gBits
                b = binaryStringToColor bBits
            in PixelRGB8 r g b
    in generateImage pixelRenderer width height
-- binaryStringToImage :: B.ByteString -> Int -> Int -> Image PixelRGB8
-- binaryStringToImage bs width height = 
--     let size = height * width * 3 * 8
--         newStr = takeCurrentSize bs size
--         pixelData = B.unpack newStr
--         pixelRenderer x y =
--             let index = (y * width + x) * 3
--                 r = binaryStringToColor (B.take 8 (B.drop index (B.pack pixelData)))
--                 g = binaryStringToColor (B.take 8 (B.drop (index + 8) (B.pack pixelData)))
--                 b = binaryStringToColor (B.take 8 (B.drop (index + 16) (B.pack pixelData)))

--             in PixelRGB8 r g b
--     in generateImage pixelRenderer width height

