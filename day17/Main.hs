{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read(..), get)
import Control.Applicative (many, Alternative(empty))
import Data.Monoid (getSum, Sum(Sum))
import Data.Set (Set)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Foldable (Foldable(fold))
import Control.Monad (guard)
import Data.Tuple.Extra (snd3, fst3, thd3)

-- * Generic

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

class Dimension d where
  type Pos d

  mkDimension :: Set (Pos d) -> d

  active :: d -> Set (Pos d)

  neighbors :: d -> Pos d -> Set (Pos d)

pos :: (Ord (Pos d), Dimension d) => d -> Pos d -> Cube
pos d p | p `Set.member` active d = Active
pos _ _ = Inactive

-- | Range from minimum - 1 to maximum + 1 of active cubes in dimension d.
forRangeOn :: Dimension d => (Pos d -> Int) -> d -> (Int -> a) -> [a]
forRangeOn f d g = map g [low - 1, low .. high + 1]
 where
  low  = minimum . Set.map f $ active d
  high = maximum . Set.map f $ active d

activeNeighbors :: (Ord (Pos d), Dimension d) => d -> Pos d -> Int
activeNeighbors d p = length $ Set.intersection (active d) (neighbors d p)

-- | A single activation cycle.
cycleDimension :: (Ord (Pos d), Dimension d) => d -> d
cycleDimension d = mkDimension . Set.filter go $ foldMap
  (neighbors d)
  (active d)
 where
  go p = case pos d p of
    Active | activeNeighbors d p == 2 || activeNeighbors d p == 3 -> True
    Inactive | activeNeighbors d p == 3 -> True
    _ -> False

countActive :: (Ord (Pos d), Dimension d) => d -> Int
countActive = length . active

-- * two dimensional

type Vec2 = (Int, Int)

newtype Dimension2 = D2 (Set Vec2)

instance Show Dimension2 where
  show d = unlines $ forY prettyY
   where
    prettyY y = show $ forX $ \x -> pos d (x, y)

    forX = forRangeOn fst d

    forY = forRangeOn snd d

instance Dimension Dimension2 where
  type Pos Dimension2 = Vec2

  mkDimension = D2

  active (D2 s) = s

  neighbors _ (x, y) = Set.fromList $ do
    xn <- [x, x + 1, x - 1]
    yn <- [y, y + 1, y - 1]
    guard ((xn, yn) /= (x, y))
    pure (xn, yn)

embed2 :: Dimension2 -> Dimension3
embed2 (D2 d2) = D3 $ Set.map v2to3 d2 where v2to3 (x, y) = (x, y, 0)

-- * three dimensional

type Vec3 = (Int, Int, Int)

newtype Dimension3 = D3 { active3 :: Set Vec3 }

instance Show Dimension3 where
  show d = concat $ forZ prettyZ
   where
    prettyZ z = "z=" ++ show z ++ "\n" ++ unlines (forY $ prettyY z)

    prettyY z y = show $ forX $ \x -> pos d (x, y, z)

    forZ = forRangeOn thd3 d

    forY = forRangeOn snd3 d

    forX = forRangeOn fst3 d

instance Dimension Dimension3 where
  type Pos Dimension3 = Vec3

  mkDimension = D3

  active (D3 s) = s

  neighbors _ (x, y, z) = Set.fromList $ do
    xn <- [x, x + 1, x - 1]
    yn <- [y, y + 1, y - 1]
    zn <- [z, z + 1, z - 1]
    guard ((xn, yn, zn) /= (x, y, z))
    pure (xn, yn, zn)

-- * four dimensional

type Vec4 = (Int, Int, Int, Int)

newtype Dimension4 = D4 (Set Vec4)

instance Dimension Dimension4 where
  type Pos Dimension4 = Vec4

  mkDimension = D4

  active (D4 s) = s

  neighbors _ (x, y, z, w) = Set.fromList $ do
    xn <- [x, x + 1, x - 1]
    yn <- [y, y + 1, y - 1]
    zn <- [z, z + 1, z - 1]
    wn <- [w, w + 1, w - 1]
    guard ((xn, yn, zn, wn) /= (x, y, z, w))
    pure (xn, yn, zn, wn)

embed3 :: Dimension3 -> Dimension4
embed3 (D3 d3) = D4 $ Set.map v3to4 d3 where v3to4 (x, y, z) = (x, y, z, 0)

type Input = Dimension2

parseInput :: String -> Input
parseInput s = mkDimension $ foldMap Set.singleton activeCubes
 where
  activeCubes = concat $ zipWith parseY [0 ..] $ lines s

  parseY y = catMaybes . zipWith (parseX y) [0 ..]

  parseX y x '#' = Just (x, y)
  parseX _ _ _   = Nothing

part1 :: Input -> String
part1 = show . countActive . (!! 6) . iterate cycleDimension . embed2

part2 :: Input -> String
part2 = show . countActive . (!! 6) . iterate cycleDimension . embed3 . embed2

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
