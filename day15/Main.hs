{-# LANGUAGE BangPatterns #-}
module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map

type Input = [Int]

type Turn = Int

type History = IntMap [Turn]

data Result = New
            | AlreadySpoken Turn Turn

historyLookup :: Int -> History -> Result
historyLookup k hist = case Map.lookup k hist of
  Just [t', t''] -> AlreadySpoken t' t''
  Just [_] -> New
  Just ts -> error (show ts)
  _ -> New

-- NOTE: keep only last 2 turns per entry
remember :: Turn -> Int -> History -> History
remember t = Map.alter (Just . maybe [t] (\ts -> [t, head ts]))

speak :: Turn -> [Int] -> Int
speak tGoal is
  | tGoal <= 0 = 0
  | tGoal <= length is = is !! (tGoal - 1)
  | otherwise  = go (length is + 1) (last is) start
 where
  start = Map.fromList $ zip is (pure <$> [1 ..])

  go !t !l !hist
    | t > tGoal = l
    | otherwise = do
      let
        res = case historyLookup l hist of
          New -> 0
          AlreadySpoken t' t'' -> t' - t''
      go (t + 1) res (remember t res hist)

part1 :: Input -> String
part1 = show . speak 2020

part2 :: Input -> String
part2 = show . speak 30000000

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
test = [0, 3, 6]

input :: Input
input = [2, 0, 6, 12, 1, 3]
