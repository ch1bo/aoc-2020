module Day01 where

findEq :: Int -> [Int] -> [(Int, Int)]
findEq n xs = go sorted (reverse sorted)
 where
  sorted = sort xs
  
  go [] _  = []
  go _  [] = []
  go (x:xs) (y:ys)
    | x == y = [] -- readed
    | x + y == n = (x,y) : go xs ys
    | x + y > n = go (x:xs) ys -- try smaller
    | x + y < n = go xs (y:ys) -- try bigger

main :: IO ()
main = do
  -- part one
  input <- map read . lines <$> readFile "input"
  let [(x,y)] = findEq 2020 input
  print $ x*y
