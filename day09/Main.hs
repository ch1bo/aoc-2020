{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)

type Input = [Integer]

parseInput :: String -> Input
parseInput = map read . lines

isValid :: [Integer] -> Integer -> Bool
isValid q z = or [ x + y == z | y <- q, x <- q, x /= y ]

findInvalid :: [Integer] -> [Integer] -> Maybe Integer
findInvalid prelude = \case
  [] -> Nothing
  (x : xs)
    | not (isValid prelude x) -> Just x
    | otherwise -> findInvalid (tail prelude <> [x]) xs

findSum :: Integer -> [Integer] -> [Integer]
findSum s xs = go [] xs
 where
  go _ [] = []
  go as (x : xs)
    | sum as == s = as
    | sum as < s  = go (x : as) xs
    | sum as > s  = go (init as) (x : xs)
    | otherwise   = []

part1 :: Int -> Input -> String
part1 window input = show $ findInvalid prelude xs
  where (prelude, xs) = splitAt window input

part2 :: Int -> Input -> String
part2 window input = show $ minimum xs + maximum xs
 where
  (prelude, rest) = splitAt window input

  (Just x) = findInvalid prelude rest

  xs = findSum x input

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 5 test
  putStrLn "Part one (input):"
  putStrLn $ part1 25 input
  putStrLn "Part two (test):"
  putStrLn $ part2 5 test
  putStrLn "Part two (input):"
  putStrLn $ part2 25 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
