module Lib
    ( logBase, matrixMultiply
    ) where

import Prelude hiding (logBase)

logBase :: Double -> Double -> Double
logBase a b = log b / log a


matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]
matrixMultiply a b 
        | colsA /= rowsB = error "Incompatible matrix dimensions"
        | otherwise = map (\row -> map (sum . zipWith(*) row) (transpose b)) a
    where 
        colsA = length (head a)
        rowsB = length b

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)


    