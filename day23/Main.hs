{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List (elemIndex)
import Data.Vector ((//), (!), (!?), Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

type Input = Cups

parseInput :: String -> Input
parseInput = cupsFromList . map (read . (: []))

newtype Cups = Cups
  { nextIndex :: Vector Int -- ^ Stores label (== index) of next (to the right) cup. Index 0 points to the "current" cup.
  }

instance Show Cups where
  show = showCups

cupsFromList :: [Int] -> Cups -- XXX PARTIAL
cupsFromList [] = error "cupsFromList: empty list"
cupsFromList l@(c : _) = Cups $ V.fromList (c : indices [1 .. length l]) -- REVIEW hard-coded 1
 where
  input = V.fromList l

  indices [] = []
  indices (i : is) =
    fromMaybe c (V.elemIndex i input >>= (!?) input . succ) : indices is

cupsToList :: Cups -> [Int]
cupsToList c = cur : go (rightOf c cur)
 where
  cur = currentCup c

  go x
    | x /= cur  = x : go (rightOf c x)
    | otherwise = []

debugCups :: Cups -> String
debugCups Cups {..} = "Cups { nextIndex = " <> show nextIndex <> "}"

showCups :: Cups -> String
showCups = go . cupsToList
  where go (c : cs) = unwords $ show c <> "!" : map show cs

currentCup :: Cups -> Int
currentCup Cups {..} = V.head nextIndex

rightOf :: Cups -> Int -> Int
rightOf Cups {..} i = nextIndex ! i

minCup :: Cups -> Int
minCup _ = 1 -- REVIEW hard-coded 1

maxCup :: Cups -> Int
maxCup Cups {..} = V.length nextIndex - 1

move :: Cups -> Cups
move c =
  Cups $! nextIndex c // [(0, next), (cur, next), (dest, x1), (x3, after)]
 where
  cur   = currentCup c
  x1    = rightOf c cur
  x2    = rightOf c x1
  x3    = rightOf c x2
  next  = rightOf c x3
  dest  = checkDestination (cur - 1)
  after = rightOf c dest

  checkDestination !x
    | x < minCup c = checkDestination $ maxCup c
    | x `elem` [x1, x2, x3] = checkDestination (x - 1)
    | otherwise    = x

shift :: Int -> [a] -> [a]
shift n xs = drop n xs <> take n xs -- partial

part1 :: Input -> String
part1 input = case elemIndex 1 res of
  Just n  -> concatMap show $ tail $ shift n res
  Nothing -> error "Can't find 1"
 where
  res   = cupsToList $ moves !! 100
  moves = iterate move input

extrapolate :: Int -> Cups -> Cups
extrapolate n c = cupsFromList $ cupsToList c <> [maxCup c + 1 .. n]

part2 :: Input -> String
part2 input = show [x1, x2, x1 * x2]
 where
  x1  = rightOf res 1

  x2  = rightOf res x1

  res = moveUntil 100000 fullInput -- TODO 10000000

  moveUntil !i !c
    | i <= 0    = c
    | otherwise = moveUntil (i - 1) (move c)

  fullInput = extrapolate 10000 input -- TODO 1000000

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 test
  putStrLn "Part one (input):"
  putStrLn $ part1 input
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  -- putStrLn "Part two (input):"
  -- putStrLn $ part2 input

test :: Input
test = parseInput "389125467"

input :: Input
input = parseInput "364297581"
