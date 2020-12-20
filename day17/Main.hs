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

class HasX a where
  getX :: a -> Int

class HasY a where
  getY :: a -> Int

class HasZ a where
  getZ :: a -> Int

class Dimension d where
  type Pos d

  mkDimension :: [Pos d] -> d

  active :: d -> Set (Pos d)

  neighbors :: d -> Pos d -> [Pos d]

  foldDimension :: Monoid m => (Pos d -> m) -> d -> m

pos :: (Ord (Pos d), Dimension d) => d -> Pos d -> Cube
pos d p | p `Set.member` active d = Active
pos _ _ = Inactive

forX :: (HasX (Pos d), Dimension d) => d -> (Int -> a) -> [a]
forX = forRangeOn getX

forY :: (HasY (Pos d), Dimension d) => d -> (Int -> a) -> [a]
forY = forRangeOn getY

forZ :: (HasZ (Pos d), Dimension d) => d -> (Int -> a) -> [a]
forZ = forRangeOn getZ

-- | Range from minimum - 1 to maximum + 1 of active cubes in dimension d.
forRangeOn :: Dimension d => (Pos d -> Int) -> d -> (Int -> a) -> [a]
forRangeOn f d g = map g [low - 1, low .. high + 1]
 where
  low  = minimum . Set.map f $ active d
  high = maximum . Set.map f $ active d

activeNeighbors :: (Ord (Pos d), Dimension d) => d -> Pos d -> Int
activeNeighbors d p = length $ filter (Active ==) $ map (pos d) $ neighbors d p

-- | A single activation cycle.
cycleDimension :: (Ord (Pos d), Dimension d) => d -> d
cycleDimension d = mkDimension $ foldDimension go d
 where
  go p = case pos d p of
    Active | activeNeighbors d p == 2 || activeNeighbors d p == 3 -> pure p
    Inactive | activeNeighbors d p == 3 -> pure p
    _ -> mempty

countActive :: (Ord (Pos d), Dimension d) => d -> Int
countActive d = getSum $ foldDimension (go . pos d) d
 where
  go Active   = Sum 1
  go Inactive = Sum 0

-- * two dimensional

type Vec2 = (Int, Int)

instance HasX Vec2 where
  getX = fst

instance HasY Vec2 where
  getY = snd

newtype Dimension2 = D2 (Set Vec2)

instance Show Dimension2 where
  show d = unlines $ forY d prettyY
    where prettyY y = show $ forX d $ \x -> pos d (x, y)

instance Dimension Dimension2 where
  type Pos Dimension2 = Vec2

  mkDimension = D2 . Set.fromList

  active (D2 s) = s

  neighbors _ (x, y) = do
    xn <- [x, x + 1, x - 1]
    yn <- [y, y + 1, y - 1]
    guard ((xn, yn) /= (x, y))
    pure (xn, yn)

  foldDimension f d = fold . forY d $ \y -> fold . forX d $ \x -> f (x, y)

embed2 :: Dimension2 -> Dimension3
embed2 (D2 d2) = D3 $ Set.map v2to3 d2 where v2to3 (x, y) = (x, y, 0)

-- * three dimensional

type Vec3 = (Int, Int, Int)

instance HasX Vec3 where
  getX (x, _, _) = x

instance HasY Vec3 where
  getY (_, y, _) = y

instance HasZ Vec3 where
  getZ (_, _, z) = z

newtype Dimension3 = D3 { active3 :: Set Vec3 }

instance Show Dimension3 where
  show d = concat $ forZ d prettyZ
   where
    prettyZ z = "z=" ++ show z ++ "\n" ++ unlines (forY d $ prettyY z)

    prettyY z y = show $ forX d $ \x -> pos d (x, y, z)

instance Dimension Dimension3 where
  type Pos Dimension3 = Vec3

  mkDimension = D3 . Set.fromList

  active (D3 s) = s

  neighbors _ (x, y, z) = do
    xn <- [x, x + 1, x - 1]
    yn <- [y, y + 1, y - 1]
    zn <- [z, z + 1, z - 1]
    guard ((xn, yn, zn) /= (x, y, z))
    pure (xn, yn, zn)

  foldDimension f d = fold . forZ d $ \z ->
    fold . forY d $ \y -> fold . forX d $ \x -> f (x, y, z)

-- * four dimensional

type Vec4 = (Int, Int, Int, Int)

instance HasX Vec4 where
  getX (x, _, _, _) = x

instance HasY Vec4 where
  getY (_, y, _, _) = y

instance HasZ Vec4 where
  getZ (_, _, z, _) = z

newtype Dimension4 = D4 (Set Vec4)

instance Dimension Dimension4 where
  type Pos Dimension4 = Vec4

  -- TODO remove?
  mkDimension = D4 . Set.fromList

  active (D4 s) = s

  neighbors _ (x, y, z, w) = do
    xn <- [x, x + 1, x - 1]
    yn <- [y, y + 1, y - 1]
    zn <- [z, z + 1, z - 1]
    wn <- [w, w + 1, w - 1]
    guard ((xn, yn, zn, wn) /= (x, y, z, w))
    pure (xn, yn, zn, wn)

  foldDimension f d = fold . forRangeOn (\(_x, _y, _z, w) -> w) d $ \w ->
    fold . forZ d $ \z ->
      fold . forY d $ \y -> fold . forX d $ \x -> f (x, y, z, w)

embed3 :: Dimension3 -> Dimension4
embed3 (D3 d3) = D4 $ Set.map v3to4 d3 where v3to4 (x, y, z) = (x, y, z, 0)

type Input = Dimension2

parseInput :: String -> Input
parseInput s = mkDimension $ foldMap pure activeCubes
 where
  activeCubes = concat $ zipWith parseY [0 ..] $ lines s

  parseY y = catMaybes . zipWith (parseX y) [0 ..]

  parseX y x '#' = Just (x, y)
  parseX _ _ _   = Nothing

part1 :: Input -> String
part1 = show . (!! 6) . map countActive . iterate cycleDimension . embed2

part2 :: Input -> String
part2 =
  show . (!! 6) . map countActive . iterate cycleDimension . embed3 . embed2

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
