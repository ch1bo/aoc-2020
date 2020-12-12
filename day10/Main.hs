{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural
import Data.List (sort)

type Input = [Natural]

parseInput :: String -> Input
parseInput = map read . lines

-- | Calculate 1-jolt and 3-jolt differences
diff13 :: Input -> (Natural, Natural)
diff13 input =
  let (_, d1, d3) = foldr go (0, 0, 0) . reverse $ sort input in (d1, d3)
 where
  go n (old, diff1, diff3)
    | n - old == 1 = (n, diff1 + 1, diff3)
    | n - old == 3 = (n, diff1, diff3 + 1)
    | otherwise    = (n, diff1, diff3)

part1 :: Input -> String
part1 input = let (d1, d3) = diff13 input in show (d1 * (d3 + 1))

part2 :: Input -> String
part2 = undefined

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
