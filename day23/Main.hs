{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List (elemIndex)

type Input = Cups

type Cups = [Int] -- TODO NonEmpty

showCups :: Cups -> String
showCups = concatMap show

parseInput :: String -> Input
parseInput = map (read . (: []))

currentCup :: Cups -> Int
currentCup = head -- XXX partial

removeClockwise :: Cups -> Int -> ([Int], Cups)
removeClockwise cups n = (take n $ tail cups, head cups : drop (n + 1) cups) -- XXX DIRTY

-- | Find cup with label smaller than current cup
destinationIndex :: Cups -> Int
destinationIndex cs = go (currentCup cs - 1)
 where
  go !label
    | label < minimum cs = go (maximum cs)
    | otherwise = case elemIndex label cs of
      Nothing -> go (label - 1)
      Just i  -> i

insertAtIndex :: Cups -> Int -> [Int] -> Cups
insertAtIndex cups index toInsert = prefix <> toInsert <> suffix
  where (prefix, suffix) = splitAt (index + 1) cups

newCurrentCup :: Cups -> Cups
newCurrentCup = shift 1

move :: Cups -> Cups
move cups = newCurrentCup $ insertAtIndex cupsRemoved destination threeCups
 where
  (threeCups, cupsRemoved) = removeClockwise cups 3

  destination = destinationIndex cupsRemoved

shift :: Int -> [a] -> [a]
shift n xs = drop n xs <> take n xs -- partial

part1 :: Input -> String
part1 input = case elemIndex 1 res of
  Just n  -> showCups $ tail $ shift n res
  Nothing -> error "Can't find 1"
 where
  res   = moves !! 100
  moves = iterate move input

part2 :: Input -> String
part2 input = show $ head res
 where
  res   = moves !! 100 -- TODO 10000000

  moves = iterate move fullInput

  fullInput = take 100000 $ extrapolate input -- TODO 1000000

main :: IO ()
main = do
  -- putStrLn "Part one (test):"
  -- putStrLn $ part1 test
  -- putStrLn "Part one (input):"
  -- putStrLn $ part1 input
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  -- putStrLn "Part two (input):"
  -- putStrLn $ part2 input

extrapolate :: [Int] -> [Int]
extrapolate xs = xs <> [maximum xs + 1 ..]

test :: Input
test = parseInput "389125467"

input :: Input
input = parseInput "364297581"
