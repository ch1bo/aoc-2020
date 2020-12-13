{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read(..), get)
import Control.Applicative (Alternative(many))
import Data.Monoid (getSum, Sum(Sum))
import Data.Vector ((!), Vector)
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

step :: GameOfLife -> GameOfLife
step g = V.imap (V.imap . go) g
 where
  go row col Empty | occupied row col == 0 = Occupied
  go row col Occupied | occupied row col >= 4 = Empty
  go _ _ x = x

  occupied r c =
    occ (r - 1) (c - 1)
      + occ (r - 1) c
      + occ (r - 1) (c + 1)
      + occ r (c - 1)
      + occ r (c + 1)
      + occ (r + 1) (c - 1)
      + occ (r + 1) c
      + occ (r + 1) (c + 1)

  occ r c | r >= 0 && r < length g && c >= 0 && c < length (g ! r) =
    case (g ! r) ! c of
      Occupied -> 1
      _ -> 0
  occ _ _ = 0

stepWhileChange :: GameOfLife -> GameOfLife
stepWhileChange g =
  let g' = step g in if g /= g' then stepWhileChange g' else g'

part1 :: Input -> String
part1 = show . count . stepWhileChange
 where
  count = getSum . foldMap (foldMap occ)

  occ Occupied = Sum 1
  occ _ = Sum 0

part2 :: Input -> String
part2 = undefined

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 test
  putStrLn "Part one (input):"
  putStrLn $ part1 input
  -- putStrLn "Part two (test):"
  -- putStrLn $ part2 test
  -- putStrLn "Part two (input):"
  -- putStrLn $ part2 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
