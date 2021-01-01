{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative (Alternative((<|>)))
import Data.List (elemIndex)
import Debug.Trace (trace)
type Input = Cups

data Cups = Cups
  { cups :: [Int]
  }

instance Show Cups where
  show Cups {..} = concatMap show cups

parseInput :: String -> Input
parseInput = Cups . map (read . (: []))

currentCup :: Cups -> Int
currentCup Cups {..} = head cups -- XXX partial

removeClockwise :: Cups -> Int -> ([Int], Cups)
removeClockwise Cups {..} n =
  (take n $ tail cups, Cups (head cups : drop (n + 1) cups)) -- XXX DIRTY

destinationIndex :: Cups -> Int
destinationIndex c@Cups {..} = go (currentCup c - 1)
 where
  go !label
    | label < minimum cups = go (maximum cups)
    | otherwise = case elemIndex label cups of
      Nothing -> go (label - 1)
      Just i  -> i

move :: Cups -> Cups
move c = let c' = Cups finalCups in trace ("cups: " ++ show c') c'
 where
  (threeCups, cupsRemoved) = removeClockwise c 3

  destination =
    trace ("cupsRemoved: " ++ show cupsRemoved) destinationIndex cupsRemoved

  (prefix, suffix) = trace
    ("destinationIndex: " ++ show destination)
    splitAt
    (destination + 1)
    (cups cupsRemoved)

  finalCups =
    trace ("pick up: " ++ show threeCups) shift 1
      $  prefix
      <> threeCups
      <> suffix

shift :: Int -> [a] -> [a]
shift n xs = drop n xs <> take n xs -- partial

part1 :: Int -> Input -> String
part1 n input = case elemIndex 1 res of
  Just n  -> show $ Cups $ tail $ shift n res
  Nothing -> error "Can't find 1"
 where
  res   = cups $ moves !! n
  moves = iterate move input

part2 :: Input -> String
part2 = undefined

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 100 test
  putStrLn "Part one (input):"
  putStrLn $ part1 100 input
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  putStrLn "Part two (input):"
  putStrLn $ part2 input

test :: Input
test = parseInput "389125467"

input :: Input
input = parseInput "364297581"
