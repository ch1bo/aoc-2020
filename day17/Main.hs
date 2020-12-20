{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read(..), get)
import Control.Applicative (many, Alternative(empty))
import Safe (atDef)
import Control.Monad (guard)
import Data.Foldable (Foldable(fold))
import Data.Monoid (getSum, Sum(Sum))

data Cube = Active | Inactive
          deriving Eq

instance Show Cube where
  showsPrec _ Active   = ('#' :)
  showsPrec _ Inactive = ('.' :)

  showList cs s = concatMap show cs ++ s

instance Read Cube where
  readPrec = get >>= \case
    '#' -> pure Active
    '.' -> pure Inactive
    _   -> empty

  readListPrec = many readPrec

newtype Dimension = D [[[Cube]]]

instance Show Dimension where
  show = prettyDimension

size :: Dimension -> Pos
size (D d) =
  let
    zlen = length d
    ylen = length (d !: 0)
    xlen = length (d !: 0 !: 0)
  in P xlen ylen zlen

prettyDimension :: Dimension -> String
prettyDimension (D d3) = unlines $ zipWith prettyZ d3 [0 ..]
  where prettyZ d2 z = "z=" ++ show z ++ "\n" ++ unlines (map show d2)

type Input = Dimension

data Pos = P Int {- x -} Int {- y -} Int {- z-}
  deriving Show

-- | Lookup with an inactive 'Cube' ('.') as default if out of bounds.
(!.) :: [Cube] -> Int -> Cube
(!.) = atDef Inactive

-- | Lookup with an empty list '[]' as default.
(!:) :: [[a]] -> Int -> [a]
(!:) = atDef []

neighbors :: Dimension -> Pos -> [Cube]
neighbors (D d) (P x y z) = do
  xn <- [x, x + 1, x - 1]
  yn <- [y, y + 1, y - 1]
  zn <- [z, z + 1, z - 1]
  guard ((xn, yn, zn) /= (x, y, z))
  pure $ d !: zn !: yn !. xn

-- | Grow dimension by 1 in all directions with inactive cubes.
grow :: Dimension -> Dimension
grow d@(D d3) = D $ growZ d3
 where
  growX xs = Inactive : xs ++ [Inactive]

  growY ys = emptyRow : map growX ys ++ [emptyRow]

  growZ zs = emptyPlane : map growY zs ++ [emptyPlane]

  emptyRow   = replicate (xlen + 2) Inactive

  emptyPlane = replicate (ylen + 2) emptyRow

  (P xlen ylen _) = size d

-- | Shrink dimension as much as possible (depending on active cubes).
shrink :: Dimension -> Dimension
shrink d@(D d3)
  | all (Inactive ==) (margin d) = D $ map (map pop . pop) $ pop d3
  | otherwise = D d3
  where pop = tail . init

margin (D d3) =
  concat (head d3) -- z head
    ++ concat (last d3) -- z last
    ++ concatMap head d3 -- y head
    ++ concatMap last d3 -- y last
    ++ concatMap (map head) d3 -- x head
    ++ concatMap (map last) d3 -- x last

-- | A single activation cycle.
cycleDimension :: Dimension -> Dimension
cycleDimension = shrink . traverseDimension go . grow
 where
  go d p Active | activeNeighbors d p == 2 || activeNeighbors d p == 3 = Active
  go d p Inactive | activeNeighbors d p == 3 = Active
  go _ _ _ = Inactive

  activeNeighbors d p = length $ filter (Active ==) $ neighbors d p

traverseDimension
  :: (Dimension -> Pos -> Cube -> Cube) -> Dimension -> Dimension
traverseDimension f d@(D d3) = D $ zipWith traversePlane [0 ..] d3
 where
  traversePlane z d2 = zipWith (traverseRow z) [0 ..] d2

  traverseRow z y xs = zipWith (\x c -> f d (P x y z) c) [0 ..] xs

foldMapDimension
  :: Monoid m => (Dimension -> Pos -> Cube -> m) -> Dimension -> m
foldMapDimension f d@(D d3) = fold $ zipWith foldPlane [0 ..] d3
 where
  foldPlane z d2 = fold $ zipWith (foldRow z) [0 ..] d2

  foldRow z y xs = fold $ zipWith (\x c -> f d (P x y z) c) [0 ..] xs

parseInput :: String -> Input
parseInput = D . pure . map read . lines

countActive :: Dimension -> Int
countActive = getSum . foldMapDimension go
 where
  go _ _ Active   = Sum 1
  go _ _ Inactive = Sum 0

part1 :: Input -> String
part1 = show . (!! 6) . map countActive . iterate cycleDimension

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
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
