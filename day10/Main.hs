{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural
import Data.List (sort)

type Input = [Natural]

parseInput :: String -> Input
parseInput = map read . lines

-- | Calculate differences
diffs :: [Natural] -> [Natural]
diffs = go 0
 where
  go _    [] = []
  go prev (x : xs) = (x - prev) : go x xs

part1 :: Input -> String
part1 input =
  let d = diffs (sort input) <> [3]
  in show (count 1 d, count 3 d, count 1 d * count 3 d)
  where count n = length . filter (== n)

part2 :: Input -> String
part2 input = show . go $ diffs $ sort input
 where
  go (1 : 1 : 1 : 1 : xs) = 7 * go xs
  go (1 : 1 : 1 : xs) = 4 * go xs
  go (1 : 1 : xs) = 2 * go xs
  go (_ : xs) = go xs
  go [] = 1

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 test
  putStrLn "Part one (input):"
  putStrLn $ part1 input
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  putStrLn "Part two (input):"
  putStrLn $ part2 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
