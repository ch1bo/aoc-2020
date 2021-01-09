{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (lift, Read(readPrec, readListPrec))
import Text.ParserCombinators.ReadP (string, choice)
import Control.Applicative (Alternative(many))
import Data.Monoid (getSum, Sum(Sum))
import Data.Tuple.Extra (both)
import Data.List (sort, group)

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

move :: Direction -> (Int, Int)
move = \case
  E  -> (0, 2)
  SE -> (1, 1)
  SW -> (1, -1)
  W  -> (0, -2)
  NW -> (-1, -1)
  NE -> (-1, 1)

coords :: Path -> (Int, Int)
coords = both getSum . foldMap (both Sum . move)

part1 :: Input -> String
part1 = show . length . blackTiles
  where blackTiles = filter odd . map length . group . sort . map coords

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
