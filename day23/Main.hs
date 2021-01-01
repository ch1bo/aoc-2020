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
  { currentIndex :: Int
  , cups :: [Int]
  }

instance Show Cups where
  show Cups {..} = unwords $ zipWith showCup cups [0 ..]
   where
    showCup c i | i == currentIndex = "(" <> show c <> ")"
    showCup c _ = show c

parseInput :: String -> Input
parseInput = Cups 0 . map (read . (: []))

currentCup :: Cups -> Int
currentCup Cups {..} = cups !! (currentIndex `mod` length cups)

removeClockwise :: Cups -> Int -> ([Int], Cups)
removeClockwise Cups {..} n =
  let
    (prefix, suffix) = splitAt (currentIndex + 1 `mod` length cups) cups
    wrap = max 0 (currentIndex + 1 + n - length cups)
  in
    ( take n suffix <> take wrap prefix
    , Cups (currentIndex - wrap) $ drop wrap prefix <> drop n suffix
    )

destinationIndex :: Cups -> Int
destinationIndex c@Cups {..} = go (currentCup c - 1)
 where
  go !label
    | label < minimum cups = go (maximum cups)
    | otherwise = case elemIndex label cups of
      Nothing -> go (label - 1)
      Just i  -> i

move :: Cups -> Cups
move c =
  let c' = Cups (finalIndex `mod` length finalCups) finalCups
  in trace ("cups: " ++ show c') c'
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
    trace ("pick up: " ++ show threeCups) prefix <> threeCups <> suffix

  finalIndex = if destination < currentIndex c
    then currentIndex c + 3 + 1
    else currentIndex c + 1

part1 :: Input -> String
part1 = show . (!! 9) . iterate move

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
test = parseInput "389125467"

input :: Input
input = parseInput "364297581"
