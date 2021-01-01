{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.List.Extra (splitOn)
import Prelude hiding (round)

type Input = Game

type Game = ([Int], [Int])

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

type Score = Int

data Result = P1Wins [Int] | P2Wins [Int]

resultScore :: Result -> Score
resultScore (P1Wins cs) = score cs
resultScore (P2Wins cs) = score cs

score :: [Int] -> Int
score cs = sum $ zipWith (*) (reverse cs) [1 ..]

play1 :: Game -> Result
play1 = \case
  (p1, []) -> P1Wins p1
  ([], p2) -> P2Wins p2
  g -> play1 $ round g

part1 :: Input -> String
part1 = show . resultScore . play1

type History = [Game]

play2 :: History -> Game -> Result
play2 hs g@(p1, _)
  | g `elem` hs = P1Wins p1
  | otherwise = case g of
    (p1, []) -> P1Wins p1
    ([], p2) -> P2Wins p2
    (c1 : p1, c2 : p2)
      | length p1 >= c1 && length p2 >= c2
      -> -- Play sub-game
         case play2 hs (take c1 p1, take c2 p2) of
        P1Wins _ -> play2 (g : hs) (p1 ++ [c1, c2], p2)
        P2Wins _ -> play2 (g : hs) (p1, p2 ++ [c2, c1])
      | otherwise
      -> -- Play normal round
         play2 (g : hs) $ round g

part2 :: Input -> String
part2 = show . resultScore . play2 []

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
