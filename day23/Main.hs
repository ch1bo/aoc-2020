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

import Data.List (elemIndex)
import Data.Vector ((//), (!), (!?), Vector)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Data.Vector.Mutable (IOVector, STVector, MVector)
import Control.Monad.ST (ST)
import Control.Monad.Primitive (PrimMonad)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vector.Mutable as MV
import Control.Monad ((>=>))

type Input = Cups

parseInput :: String -> Input
parseInput = unsafePerformIO . cupsFromList . map (read . (: []))
{-# NOINLINE parseInput #-}

newtype Cups = Cups
  { nextIndex :: IOVector Int -- ^ Stores label (== index) of next (to the right) cup. Index 0 points to the "current" cup.
  }

cupsFromList :: [Int] -> IO Cups -- XXX PARTIAL
cupsFromList [] = error "cupsFromList: empty list"
cupsFromList l@(c : _) = do
  v <- V.thaw $ V.fromList (c : indices [1 .. length l]) -- REVIEW hard-coded 1
  pure $ Cups v
 where
  input = V.fromList l

  indices [] = []
  indices (i : is) =
    fromMaybe c (V.elemIndex i input >>= (!?) input . succ) : indices is

cupsToList :: Cups -> Int -> IO [Int]
cupsToList c start = do
  (start :) <$> (rightOf c start >>= go)
 where
  go x
    | x /= start = (x :) <$> (rightOf c x >>= go)
    | otherwise  = pure []

debugCups :: Cups -> IO String
debugCups c@Cups {..} =
  unwords <$> mapM (fmap show . MV.read nextIndex) [0 .. maxCup c]

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

shift :: Int -> [a] -> [a]
shift n xs = drop n xs <> take n xs -- partial

iterateNM :: Monad m => (a -> m a) -> Int -> a -> m a
iterateNM f !i !a
  | i <= 0    = pure a
  | otherwise = f a >>= iterateNM f (i - 1)

part1 :: Input -> IO String
part1 input = do
  res <- iterateNM move 100 input
  concatMap show . tail <$> cupsToList res 1

-- | Add elements up to n onto the "end" (left of currentCup)
extrapolate :: Int -> Cups -> IO Cups
extrapolate n c@(Cups vold) = do
  vnew <- MV.grow vold addedLen
  cur  <- currentCup c
  end  <- leftOf c cur
  -- add increasing cups where previous cup is always to the left
  mapM_ (\i -> MV.write vnew i (i + 1)) [maxCup c + 1 .. n]
  -- point to added cups
  MV.write vnew end (maxCup c + 1)
  -- point to start
  MV.write vnew n cur
  pure $ Cups vnew
  where addedLen = n - MV.length vold + 1

part2 :: Input -> IO String
part2 input = do
  fullInput <- extrapolate 1000000 input
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

test :: Input
test = parseInput "389125467"

input :: Input
input = parseInput "364297581"
