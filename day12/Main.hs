{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readPrec, get)
import Control.Monad.State (gets, modify, execState)
import Data.Tuple.Extra (first3, second3, third3, fst3)
import Control.Monad (replicateM_)

type Input = [Action]

data Action = Move Direction Int
            | L Int
            | R Int
            | F Int
            deriving Show

instance Read Action where
  readPrec = get >>= \case
    'L' -> L <$> readPrec
    'R' -> R <$> readPrec
    'F' -> F <$> readPrec
    c   -> Move (read [c]) <$> readPrec

data Direction = N | S | E | W
               deriving (Read, Show)

parseInput :: String -> Input
parseInput = map read . lines

turn :: Int -> Direction -> Direction
turn deg d
  | deg > 90 = turn (deg - 90) $ turn 90 d
  | deg < 0 = turn (360 + deg) d
  | otherwise = case d of
    N -> E
    E -> S
    S -> W
    W -> N

interpretDirect :: [Action] -> (Direction, Int, Int)
interpretDirect as = execState (mapM go as) (E, 0, 0)
 where
  go = \case
    Move N x -> north x
    Move S x -> north (-x)
    Move E x -> east x
    Move W x -> east (-x)
    L x -> turn' (-x)
    R x -> turn' x
    F x -> gets fst3 >>= \case
      N -> north x
      E -> east x
      S -> north (-x)
      W -> east (-x)

  turn' delta = modify (first3 $ turn delta)

  north delta = modify (second3 (+ delta))

  east delta = modify (third3 (+ delta))

distance :: (a, Int, Int) -> Int
distance (_, n, e) = fromIntegral (abs n) + fromIntegral (abs e)

part1 :: Input -> String
part1 = show . distance . interpretDirect

data Waypoint = WP Int Int
              deriving Show

interpretWaypoint :: [Action] -> (Waypoint, Int, Int)
interpretWaypoint as = execState (mapM go as) (WP 1 10, 0, 0)
 where
  go = \case
    Move N x -> wpNorth x
    Move S x -> wpNorth (-x)
    Move E x -> wpEast x
    Move W x -> wpEast (-x)
    L x -> rotate (-x)
    R x -> rotate x
    F x -> replicateM_ x moveToWP

  wpNorth d = modify (first3 (\(WP n e) -> WP (n + d) e))

  wpEast d = modify (first3 (\(WP n e) -> WP n (e + d)))

  rotate deg
    | deg < 0   = rotate (360 + deg)
    | deg > 90  = rotate 90 >> rotate (deg - 90)
    | otherwise = -- rotate by 90
                  modify (first3 (\(WP n e) -> WP (-e) n))

  moveToWP = modify $ \(wp@(WP n' e'), n, e) -> (wp, n + n', e + e')

part2 :: Input -> String
part2 = show . distance . interpretWaypoint

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
