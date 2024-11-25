{-# LANGUAGE FlexibleContexts #-}

type Step = (String,Int)
type Steps = [Step]


botStep :: (Int, Int, Int) -> (Int, Int, Int) -- (n, k, count)
botStep (n, k, count)
  | n < k = (n, k , n)
  | n > k && count == 1 = (n, k, 1) 
  | otherwise = 
    if (n - count) `mod` (k + 1) == 0 then (n, k, count)
    else botStep(n, k, count - 1)


check :: Int -> Int -> IO Int --[k, count]
check k count  = 
    if count > 0 && count <= k then do return count
    else do
      putStrLn ("Ошибка ввода. Попробуйте снова")
      input <- getLine
      let pl = read input :: Int
      check k pl


takeFirstOf3 :: (Int, Int, Int) -> Int
takeFirstOf3 (_,_,count) = count

takeFirstOf2 :: (String, Int) -> String
takeFirstOf2 (name,_) = name

step :: (Int, Int, Steps) -> IO (Int, Int, Steps)
step (n, k, history) 
  | n == 0  = do
    if takeFirstOf2(last history) == "Bot" then  
        putStrLn ("Бот выйграл")
    else 
        putStrLn ("Вы выйграли!!")
    return (n, k, history)

  | n > 0 = do
      let bot = takeFirstOf3(botStep (n, k, k))
          botName = "Bot"
          playerName = "Player"
          newn = n - bot 
          newhistory = history ++ [(botName, bot)]
      putStrLn ("Было "++ show n ++". Бот взял " ++ show bot ++ " камня(ей)") 
      if newn == 0 then do
        putStrLn ("Бот выйграл")
        return (newn, k, newhistory)
      else do
        putStrLn ("Сколько камней возьмете вы? В куче сейчас " ++ show newn ++ ", ограничение - " ++ show k)
        input <- getLine
        let pl = read input :: Int
        finalpl <- check k pl
        let newhistory2 = newhistory ++ [(playerName, finalpl)]
        step (newn - finalpl, k, newhistory2)

printLn :: [Step] -> IO()
printLn = mapM_ (\(name, count) -> putStrLn(name ++ ": " ++ show count)) 

main :: IO()
main = do
  putStrLn "Введите количество камней в куче:"
  input <- getLine
  let n = read input :: Int
  putStrLn "Введите ограничение на взятие камней:"
  input2 <- getLine
  let k = read input2 ::Int
  let history =[] :: [Step]
  (n', k',history') <- step (n, k, history)
  putStrLn ""
  putStrLn "История ходов:"
  printLn history'


