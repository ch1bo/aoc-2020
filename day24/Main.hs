{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (lift, Read(readPrec, readListPrec))
import Text.ParserCombinators.ReadP (string, choice)
import Control.Applicative (Alternative(many))
import Data.Monoid (getSum, Sum(Sum))
import Data.Tuple.Extra (both)
import Data.List (foldl', sort, group)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Direction = E | SE | SW | W | NW | NE
  deriving (Eq, Show)

instance Read Direction where
  readPrec = lift $ choice
    [ E <$ string "e"
    , SE <$ string "se"
    , SW <$ string "sw"
    , W <$ string "w"
    , NW <$ string "nw"
    , NE <$ string "ne"
    ]

  readListPrec = many readPrec

type Path = [Direction]

type Input = [Path]

parseInput :: String -> Input
parseInput = map read . lines

move :: (Int, Int) -> Direction -> (Int, Int)
move (y, x) = \case
  E  -> (y, x + 2)
  SE -> (y + 1, x + 1)
  SW -> (y + 1, x - 1)
  W  -> (y, x - 2)
  NW -> (y - 1, x - 1)
  NE -> (y - 1, x + 1)

directions :: [Direction]
directions = [E, SE, SW, W, NW, NE]

coords :: Path -> (Int, Int)
coords = foldl' move (0, 0)

blackTiles :: Input -> [(Int, Int)]
blackTiles = mapMaybe go . group . sort . map coords
 where
  go l@(x : _) | odd (length l) = Just x
  go _ = Nothing

part1 :: Input -> String
part1 = show . length . blackTiles

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors p = map (move p) directions

data Color = Black | White

nextDay :: Set (Int, Int) -> Set (Int, Int)
nextDay blacks = Set.filter
  (\x -> willBeBlack (color x) (blackNeighbors x))
  relevantTiles
 where
  relevantTiles = blacks <> foldMap (Set.fromList . neighbors) blacks

  color x = if x `elem` blacks then Black else White

  blackNeighbors =
    Set.size . Set.intersection blacks . Set.fromList . neighbors

  willBeBlack Black x | x == 0 || x > 2 = False
  willBeBlack Black _ = True
  willBeBlack White 2 = True
  willBeBlack White _ = False

part2 :: Input -> String
part2 =
  unlines
    . map (\(i, n) -> "Day " <> show i <> ": " <> show (Set.size n))
    . take 101
    . zip [0 ..]
    . iterate nextDay
    . Set.fromList
    . blackTiles

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
