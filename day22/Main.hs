{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import GHC.Natural (Natural)
import Data.List.Extra (splitOn)
import Prelude hiding (round)

type Input = Game

type Game = ([Natural], [Natural])

parseInput :: String -> Input
parseInput s = (parse p1, parse p2)
 where
  [p1, p2] = splitOn "\n\n" s

  parse    = map read . tail . lines

round :: Game -> Game
round ([], p2) = ([], p2)
round (p1, []) = (p1, [])
round (c1 : p1, c2 : p2)
  | c1 > c2   = (p1 ++ [c1, c2], p2)
  | c1 < c2   = (p1, p2 ++ [c2, c1])
  | otherwise = error "undefined game state"

type Score = Natural

play :: Game -> Score
play = \case
  (p1, []) -> score p1
  ([], p2) -> score p2
  g -> play $ round g
  where score cs = sum $ zipWith (*) (reverse cs) [1 ..]

part1 :: Input -> String
part1 = undefined

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
