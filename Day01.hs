module Day01 where

findEq2 :: Int -> [Int] -> [(Int, Int)]
findEq2 n = filter (\(a,b) -> a+b == n) . tuples

findEq3 :: Int -> [Int] -> [(Int, Int, Int)]
findEq3 n = filter (\(a,b,c) -> a+b+c == n) . triples

tuples [] = []
tuples (x:xs) =
  [(x,y) | y <- xs] <> tuples xs

triples :: [a] -> [(a,a,a)]
triples [] = []
triples (x:xs) =
  [(x,y,z) | (y,z) <- tuples xs] <> triples xs
  
main :: IO ()
main = do
  -- let input = [ 1721
  --             , 979
  --             , 366
  --             , 299
  --             , 675
  --             , 1456
  --             ]
  putStrLn "part one"
  input <- map read . lines <$> readFile "input"
  let [(x,y)] = findEq2 2020 input
  print $ x*y
  putStrLn "part two"
  let [(x,y,z)] = findEq3 2020 input
  print $ x*y*z
