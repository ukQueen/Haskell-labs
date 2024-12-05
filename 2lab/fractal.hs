type Point = (Double, Double)
type Line = (Point, Point)


nextLevel :: Line -> [Line]
nextLevel ((x1,y1), (x2, y2)) = 
  let dx = x2 - x1
      dy = y2 - y1
      len = (sqrt (dx^2 + dy^2) / sqrt 2) 
      deg1 = atan2 dy dx + pi / 4
      deg2 = atan2 dy dx - pi / 4
      p1 = (x2 + len * cos deg1, y2 + len * sin deg1)
      p2 = (x2 + len * cos deg2, y2 + len * sin deg2)
  in [((x2,y2),p1), ((x2,y2), p2)]


generateFractal :: Int -> [Line]
generateFractal 0 = [((2,0), (2,1))]
generateFractal n = 
  let prevLevel = generateFractal (n-1)
  in concatMap nextLevel prevLevel

groupByLevels :: Int -> [[Line]]
groupByLevels 0 = [[((2,0),(2,1))]]
groupByLevels n = 
  let prevLevels = groupByLevels (n-1)
      nextLines = concatMap nextLevel (last prevLevels)
  in  prevLevels ++ [nextLines] 

showLineList :: [Line] -> String
showLineList lines = 
  "[" ++ concatMap (\((x1, y1), (x2, y2)) -> "((" ++ show x1 ++ "," ++ show y1 ++ "),(" ++ show x2 ++ "," ++ show y2 ++ ")),") lines ++ "]\n"


printLn :: [[Line]] -> String
printLn a = "[\n" ++ concatMap showLineList a ++ "]"


main :: IO()
main = do
  putStrLn "Введите глубину фрактала:"
  input <- getLine
  let steps = read input :: Int
  let result = printLn (groupByLevels steps)
  putStrLn result
