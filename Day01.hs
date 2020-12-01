{-# LANGUAGE TupleSections #-}
module Day01 where

import Data.List (nubBy, sort)
import Control.Monad (guard)

findEq2 :: Int -> [Int] -> [(Int, Int)]
findEq2 n xs = go sorted (reverse sorted)
 where
  sorted = sort xs
  
  go [] _  = []
  go _  [] = []
  go (x:xs) (y:ys)
    | x == y = [] -- end
    | x + y == n = (x,y) : go xs ys
    | x + y > n = go (x:xs) ys -- try smaller
    | x + y < n = go xs (y:ys) -- try bigger

-- TODO triples produces O(n^3) output .. not going to work

findEq3 :: Int -> [Int] -> [(Int, Int, Int)]
findEq3 n xs = do
  filter ((==n) . sum3) . nubBy (\a b -> sum3 a == sum3 b) $ triples xs

sum3 :: Num a => (a,a,a) -> a
sum3 (a,b,c) = a + b + c

tuples xs = do
  a <- xs
  b <- xs
  guard $ a /= b
  pure (a,b)

triples xs = do
  (a,b) <- tuples xs
  c <- xs
  guard $ c /= a && c /= b
  pure (a,b,c)
  
main :: IO ()
main = do
  -- let input = [ 1721
  --             , 979
  --             , 366
  --             , 299
  --             , 675
  --             , 1456
  --             ]
  -- part one
  input <- map read . lines <$> readFile "input"
  let [(x,y)] = findEq2 2020 input
  print $ x*y
  -- part two
  print $ findEq3 2020 input
