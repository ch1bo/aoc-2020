{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read(..), get)
import Control.Applicative (Alternative(many))
import Data.Monoid (getSum, Sum(Sum))
import Data.Vector ((!?), (!), Vector)
import qualified Data.Vector as V

type Input = GameOfLife

type GameOfLife = Vector (Vector Cell)

data Cell = Empty | Occupied | Floor
          deriving Eq

instance Show Cell where
  showsPrec _ Empty    s = 'L' : s
  showsPrec _ Occupied s = '#' : s
  showsPrec _ Floor    s = '.' : s

  showList l s = foldr shows s l

instance Read Cell where
  readPrec = get >>= \case
    '.' -> pure Floor
    'L' -> pure Empty
    '#' -> pure Occupied

  readListPrec = many readPrec

parseInput :: String -> Input
parseInput = V.fromList . map (V.fromList . read) . lines

data Direction = TL | T | TR | L | R | BL | B | BR

allDirections :: [Direction]
allDirections = [TL, T, TR, L, R, BL, B, BR]

data Pos = Pos { y :: !Int, x :: !Int }

getCell :: Pos -> GameOfLife -> Maybe Cell
getCell p g = (g !? y p) >>= (!? x p)

shift :: Pos -> Direction -> Pos
shift (Pos y x) = \case
  TL -> Pos (y - 1) (x - 1)
  T  -> Pos (y - 1) x
  TR -> Pos (y - 1) (x + 1)
  L  -> Pos y (x - 1)
  R  -> Pos y (x + 1)
  BL -> Pos (y + 1) (x - 1)
  B  -> Pos (y + 1) x
  BR -> Pos (y + 1) (x + 1)

step :: GameOfLife -> GameOfLife
step g = V.imap (V.imap . go) g
 where
  go row col Empty | occupied row col == 0 = Occupied
  go row col Occupied | occupied row col >= 4 = Empty
  go _ _ x = x

  occupied r c = sum $ map (occ . shift (Pos r c)) allDirections

  occ p = case getCell p g of
    Just Occupied -> 1
    _ -> 0

repeatUntilEq :: Eq a => (a -> a) -> a -> a
repeatUntilEq f a = let a' = f a in if a /= a' then repeatUntilEq f a' else a'

countOccupied :: GameOfLife -> Int
countOccupied = getSum . foldMap (foldMap occ)
 where
  occ Occupied = Sum 1
  occ _ = Sum 0

part1 :: Input -> String
part1 = show . countOccupied . repeatUntilEq step

-- | Look for an occupied seat in direction
look :: Pos -> GameOfLife -> Direction -> Bool
look p g dir = case getCell (shift p dir) g of
  Just Occupied -> True
  Just Empty -> False
  Just Floor -> look (shift p dir) g dir
  Nothing -> False

-- | Look and count occupied seats around
lookAround :: Pos -> GameOfLife -> Int
lookAround p g =
  getSum $ foldMap (\dir -> if look p g dir then Sum 1 else Sum 0) allDirections

step2 :: GameOfLife -> GameOfLife
step2 g = V.imap (V.imap . go) g
 where
  go row col Empty | lookAround (Pos row col) g == 0 = Occupied
  go row col Occupied | lookAround (Pos row col) g >= 5 = Empty
  go _ _ x = x

part2 :: Input -> String
part2 = show . countOccupied . repeatUntilEq step2

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
