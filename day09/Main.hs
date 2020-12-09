{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)

type Input = [Integer]

parseInput :: String -> Input
parseInput = map read . lines

validate :: [Integer] -> Integer -> Bool
validate q z = or [ x + y == z | y <- q, x <- q, x /= y ]

findInvalid :: Int -> Input -> Maybe Integer
findInvalid window input = go prelude xs
 where
  prelude = take window input

  xs = drop window input

  go _ [] = Nothing
  go q (x : xs)
    | not (validate q x) = Just x
    | otherwise = go (shift q x) xs

  shift xs x = tail xs <> [x]

findSum :: Integer -> [Integer] -> [Integer]
findSum _ [] = []
findSum s (x : xs) = case go 0 [] (x : xs) of
  [] -> findSum s xs
  as -> reverse as
 where
  go _ _ [] = []
  go acc as (x : xs)
    | acc == s     = as
    | acc + x <= s = go (acc + x) (x : as) xs
    | otherwise    = []

part1 :: Int -> Input -> String
part1 window input = show $ findInvalid window input

part2 :: Int -> Input -> String
part2 window input = case findInvalid window input of
  Nothing -> error "nothing invalid"
  Just x  -> case findSum x input of
    [] -> error "no sequence found"
    xs -> show $ minimum xs + maximum xs

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
