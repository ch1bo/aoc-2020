{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.List.Extra (splitOn)
import Data.List (sort)
import Data.Foldable (minimumBy)
import Data.Function (on)

type Input = (Time, [Bus])

type Time = Int
type Bus = Int

parseInput :: String -> Input
parseInput s =
  let
    (t : x : _) = splitOn "\n" s
    bs = filter (/= "x") $ splitOn "," x
  in (read t, map read bs)

part1 :: Input -> String
part1 (t, bs) =
  let
    departures = map (\b -> (t `div` b + 1) * b) bs
    (nextTime, nextBus) = minimumBy (compare `on` fst) $ zip departures bs
    wait = nextTime - t
  in show (nextBus * wait)

part2 :: Input -> String
part2 = undefined

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 test
  putStrLn "Part one (input):"
  putStrLn $ part1 input
  -- putStrLn "Part two (test):"
  -- putStrLn $ part2 test
  -- putStrLn "Part two (input):"
  -- putStrLn $ part2 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
