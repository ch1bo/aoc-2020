{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Vector ((!))
import qualified Data.Vector as V
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MV
import Data.Foldable (for_)

newtype Cups = Cups
  { nextIndex :: IOVector Int -- ^ Stores label (== index) of next (to the
                              -- right) cup. Index 0 points to the "current"
                              -- cup.
  }

cupsFromList :: [Int] -> IO Cups
cupsFromList [] = error "cupsFromList: empty list"
cupsFromList cs = do
  v <- MV.new (len + 1)
  MV.write v 0 (input ! 0) -- current cup "pointer"
  for_ [0 .. len - 1] $ \i -> do
    let cup  = input ! i
    let next = input ! ((i + 1) `mod` len) -- TODO use Finite
    MV.write v cup next
  pure $ Cups v
 where
  input = V.fromList cs
  len   = V.length input

cupsToList :: Cups -> Int -> IO [Int]
cupsToList c start = do
  (start :) <$> (rightOf c start >>= go)
 where
  go x
    | x /= start = (x :) <$> (rightOf c x >>= go)
    | otherwise  = pure []

showCups :: Cups -> IO String
showCups c = do
  cur <- currentCup c
  go <$> cupsToList c cur
  where go (c : cs) = unwords $ show c <> "!" : map show cs

currentCup :: Cups -> IO Int
currentCup Cups {..} = MV.read nextIndex 0

-- | O(1) Cup right of given cup
rightOf :: Cups -> Int -> IO Int
rightOf Cups {..} = MV.read nextIndex

-- | O(n) Cup left of given cup, i.e find cup has next index i.
leftOf :: Cups -> Int -> IO Int
leftOf c i = currentCup c >>= go
  where go x = rightOf c x >>= \next -> if next == i then pure x else go next

minCup :: Cups -> Int
minCup _ = 1 -- REVIEW hard-coded 1

maxCup :: Cups -> Int
maxCup Cups {..} = MV.length nextIndex - 1

move :: Cups -> IO Cups
move c@Cups {..} = do
  cur  <- currentCup c
  x1   <- rightOf c cur
  x2   <- rightOf c x1
  x3   <- rightOf c x2
  next <- rightOf c x3
  let dest = checkDestination [x1, x2, x3] (cur - 1)
  after <- rightOf c dest
  mapM_
    (uncurry $ MV.write nextIndex)
    [(0, next), (cur, next), (dest, x1), (x3, after)]
  pure c
 where
  checkDestination bl !x
    | x < minCup c = checkDestination bl $ maxCup c
    | x `elem` bl  = checkDestination bl (x - 1)
    | otherwise    = x

iterateNM :: Monad m => (a -> m a) -> Int -> a -> m a
iterateNM f !i !a
  | i <= 0    = pure a
  | otherwise = f a >>= iterateNM f (i - 1)

part1 :: Input -> IO String
part1 input = do
  res <- iterateNM move 100 =<< cupsFromList input
  concatMap show . tail <$> cupsToList res 1

extrapolate :: Int -> [Int] -> [Int]
extrapolate n xs = xs <> [length xs + 1 .. n]

part2 :: Input -> IO String
part2 input = do
  fullInput <- cupsFromList $ extrapolate 1000000 input
  res <- iterateNM move 10000000 fullInput
  x1  <- rightOf res 1
  x2  <- rightOf res x1
  pure $ show [x1, x2, x1 * x2]

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn =<< part1 test
  putStrLn "Part one (input):"
  putStrLn =<< part1 input
  putStrLn "Part two (test):"
  putStrLn =<< part2 test
  putStrLn "Part two (input):"
  putStrLn =<< part2 input

type Input = [Int]

parseInput :: String -> Input
parseInput = map (read . (: []))

test :: Input
test = parseInput "389125467"

input :: Input
input = parseInput "364297581"
