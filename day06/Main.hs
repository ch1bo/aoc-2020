module Main where

import Text.Printf (printf)

type Input = String

parseInput :: String -> Input
parseInput = id

part1 :: Input -> Int
part1 = error "not implemented"

part2 :: Input -> Int
part2 = error "not implemented"

main :: IO ()
main = go "test" >> go "input"
  where
    go fn = do
    testInput <- parseInput <$> readFile fn
    printf "Part one (%s):\n" fn
    print $ part1 testInput
    printf "Part two (%s):\n" fn
    print $ part2 testInput
