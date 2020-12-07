module Main where

import           Data.List                      ( intersect
                                                , nub
                                                )
import           Data.List.Extra                ( splitOn )
import           Text.Printf                    ( printf )

type Input = [[String]]

type Output = Int

parseInput :: String -> Input
parseInput = map (filter (/= "") . splitOn "\n") . splitOn "\n\n"

part1 :: Input -> Output
part1 = sum . map (length . nub . concat)

part2 :: Input -> Output
part2 = sum . map (length . go)
 where
  go (x : y : xs) = x `intersect` go (y : xs)
  go [x         ] = x
  go []           = []

main :: IO ()
main = do
  go "test"
  go "input"
 where
  go fn = do
    testInput <- parseInput <$> readFile fn
    printf "Part one (%s):\n" fn
    print $ part1 testInput
    printf "Part two (%s):\n" fn
    print $ part2 testInput
