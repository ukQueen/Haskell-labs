import Test.QuickCheck
import Lib
import Prelude hiding (logBase)
import Control.Monad (replicateM)




prop_reverse :: Double -> Double -> Property
prop_reverse b x = b > 0 && x > 0 && b /= 1 ==> almostEqual first second 1e-9
    where 
        first = logBase b (b ** x)
        second = x

prop_change :: Double -> Double -> Double -> Property
prop_change a b c = a > 0 && b > 0 && c > 0 && c /= 1 && a /= 1 ==> almostEqual first second 1e-9
    where 
        first = logBase a b
        second = logBase c b / logBase c a

prop_one :: Double -> Property
prop_one b = b > 0 && b /= 1 ==> almostEqual first second 1e-9
    where 
        first = logBase b 1
        second = 0

almostEqual :: Double -> Double -> Double -> Bool
almostEqual a b epsilon = abs (a - b) < epsilon

iMatrix :: Num a => Int -> [[a]]
iMatrix n = [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

genMatrix :: (Arbitrary a) => Int -> Int -> Gen [[a]]
genMatrix rows cols = replicateM rows (vectorOf cols arbitrary)

genAssociativeMatrices :: (Arbitrary a) => Gen ([[a]], [[a]], [[a]])
genAssociativeMatrices = do
    m <- chooseInt (1, 5)  -- строки в матрице A
    n <- chooseInt (1, 5)  -- столбцы в A и строки в B
    p <- chooseInt (1, 5)  -- столбцы в B и строки в C
    q <- chooseInt (1, 5)  -- столбцы в C
    a <- genMatrix m n
    b <- genMatrix n p
    c <- genMatrix p q
    return (a, b, c)

genDistributiveMatrices :: (Arbitrary a) => Gen ([[a]], [[a]], [[a]])
genDistributiveMatrices = do
    m <- chooseInt (1, 5)  -- строки в матрице A
    n <- chooseInt (1, 5)  -- столбцы в A и строки в В и С
    q <- chooseInt (1, 5)  -- столбцы в В и столбцы в С
    a <- genMatrix m n
    b <- genMatrix n q
    c <- genMatrix n q
    return (a, b, c)

genMultiplicationMatrices :: (Arbitrary a, Num a) => Gen ([[a]], [[a]])
genMultiplicationMatrices = do
    m <- chooseInt (1, 5)  -- строки в матрице A
    n <- chooseInt (1, 5)  -- столбцы в A и строки и столбцы в I 
    a <- genMatrix m n
    b <- return (iMatrix n)
    return (a, b)



prop_associativity_int :: Property
prop_associativity_int =
    forAll (genAssociativeMatrices :: Gen ([[Int]], [[Int]], [[Int]])) $ \(a, b, c) ->
        matrixMultiply (matrixMultiply a b) c == matrixMultiply a (matrixMultiply b c)


prop_associativity_double :: Property
prop_associativity_double =
    forAll (genAssociativeMatrices :: Gen ([[Double]], [[Double]], [[Double]])) $ \(a, b, c) ->
        let left  = matrixMultiply (matrixMultiply a b) c
            right = matrixMultiply a (matrixMultiply b c)
        in almostEqualMatrices left right 1e-9

almostEqualMatrices :: [[Double]] -> [[Double]] -> Double -> Bool
almostEqualMatrices left right epsilon =
    if null left || null right then
        left == right 
    else
        let matrixLeft = concat left
            matrixRight = concat right
        in length matrixLeft == length matrixRight &&
           all (\(l, r) -> almostEqual l r epsilon) (zip matrixLeft matrixRight)



prop_distributivity_int :: Property
prop_distributivity_int =
    forAll (genDistributiveMatrices :: Gen ([[Int]], [[Int]], [[Int]])) $ \(a, b, c) ->
        matrixMultiply a ( zipWith(zipWith(+)) b c) == zipWith(zipWith(+)) (matrixMultiply a b) (matrixMultiply a c)

prop_distributivity_double :: Property
prop_distributivity_double =
    forAll (genDistributiveMatrices :: Gen ([[Double]], [[Double]], [[Double]])) $ \(a, b, c) ->
        let left  = matrixMultiply a ( zipWith(zipWith(+)) b c)
            right = zipWith(zipWith(+)) (matrixMultiply a b) (matrixMultiply a c)
        in almostEqualMatrices left right 1e-9


prop_multiplication_int :: Property
prop_multiplication_int =
    forAll (genMultiplicationMatrices :: Gen ([[Int]], [[Int]])) $ \(a, b) ->
        matrixMultiply a b == a

prop_multiplication_double :: Property
prop_multiplication_double =
    forAll (genMultiplicationMatrices :: Gen ([[Double]], [[Double]])) $ \(a, b) ->
        let left  = matrixMultiply a b
            right = a
        in almostEqualMatrices left right 1e-9


main :: IO ()
main = do
    putStrLn("Тест prop_reverse:")
    quickCheck prop_reverse

    putStrLn("\nТест prop_change:")
    quickCheck prop_change

    putStrLn("\nТест prop_one:")
    quickCheck prop_one

    putStrLn("\n-----------------------------------")

    putStrLn("Тест prop_reverse:")
    quickCheckWith stdArgs {maxSuccess = 1000}  prop_reverse

    putStrLn("\nТест prop_change:")
    quickCheckWith stdArgs {maxSuccess = 1000}  prop_change

    putStrLn("\nТест prop_one:")
    quickCheckWith stdArgs {maxSuccess = 1000}  prop_one

    putStrLn("\n-----------------------------------")

    putStrLn("\nТест prop_associativity (with Int):")
    quickCheck prop_associativity_int 

    putStrLn("\nТест prop_associativity (with Double):")
    quickCheck prop_associativity_double

    putStrLn("\nТест prop_distributivity (with Int):")
    quickCheck prop_distributivity_int 

    putStrLn("\nТест prop_distributivity (with Double):")
    quickCheck prop_distributivity_double

    putStrLn("\nТест prop_multiplication (with Int):")
    quickCheck prop_multiplication_int 

    putStrLn("\nТест prop_multiplication (with Double):")
    quickCheck prop_multiplication_double