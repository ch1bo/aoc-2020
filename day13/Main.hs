{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.List.Extra (splitOn)
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.Tuple.Extra (first)
import Debug.Trace (trace)
import Text.Printf (printf)

type Input = (Time, [Bus])

type Time = Integer

type Bus = (Integer, Integer) -- (id, index)

parseInput :: String -> Input
parseInput s =
  let
    (t : x : _) = splitOn "\n" s
    bs = filter ((/= "x") . fst) $ zip (splitOn "," x) [0 ..]
  in (read t, map (first read) bs)

part1 :: Input -> String
part1 (t, bs) =
  let
    bs'  = map fst bs
    departures = map (\b -> (t `div` b + 1) * b) bs'
    (nextTime, nextBus) = minimumBy (compare `on` fst) $ zip departures bs'
    wait = nextTime - t
  in show (nextBus * wait)

part2 :: Input -> String
part2 (_, bs) = show $ go 1 1 bs
 where
  go !i _ [] = i
  go !i inc ((bid, idx) : next)
    | (i + idx) `mod` bid == 0 = go i (lcm bid inc) next
    | otherwise = go (i + inc) inc bs

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
