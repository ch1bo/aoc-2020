{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read
import Control.Monad.State.Strict (modify, execState)

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

turnR :: Direction -> Int -> Direction
turnR N 90 = E
turnR E 90 = S
turnR S 90 = W
turnR W 90 = N
turnR d deg
  | deg > 90 = turnR (turnR d 90) (deg - 90)
  | deg < 0  = error "turnR negative"

turnL :: Direction -> Int -> Direction
turnL N 90 = W
turnL W 90 = S
turnL S 90 = E
turnL E 90 = N
turnL d deg
  | deg > 90 = turnL (turnL d 90) (deg - 90)
  | deg < 0  = error "turnL negative"

interpret :: [Action] -> (Direction, Int, Int)
interpret as = execState (mapM go as) (E, 0, 0)
 where
  go = \case
    Move N x -> modify (north x)
    Move S x -> modify (north (-x))
    Move E x -> modify (east x)
    Move W x -> modify (east (-x))
    L x -> modify (\(d, n, e) -> (turnL d x, n, e))
    R x -> modify (\(d, n, e) -> (turnR d x, n, e))
    F x -> modify (move x)

  north delta (f, n, e) = (f, n + delta, e)

  east delta (f, n, e) = (f, n, e + delta)

  move delta (N, n, e) = (N, n + delta, e)
  move delta (E, n, e) = (E, n, e + delta)
  move delta (S, n, e) = (S, n - delta, e)
  move delta (W, n, e) = (W, n, e - delta)

distance :: (Direction, Int, Int) -> Int
distance (_, n, e) = fromIntegral (abs n) + fromIntegral (abs e)

part1 :: Input -> String
part1 = show . distance . interpret

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
