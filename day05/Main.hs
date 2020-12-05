{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Function (fix)

seat :: String -> Int
seat input =
  let r = row 128 $ take 7 input
      c = col 8 $ drop 7 input
  in  r * 8 + c
  where
    row = divide 'F' 'B'
    col = divide 'L' 'R'

    divide l h = fix $ \rec i -> \case
      [] -> i -1
      (x:xs)
        | x == l -> rec (i `div` 2) xs
        | x == h -> i `div` 2 + rec (i `div` 2) xs

part1 :: String -> String
part1 input = show . maximum $ map seat $ lines input

part2 :: String -> String
part2 = undefined

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "part one"
  putStrLn $ part1 input
  putStrLn "part two"
  putStrLn $ part2 input
