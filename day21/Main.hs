{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.List.Extra (splitOn)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.List as List
import Data.List (nub, (\\))
import Data.Foldable (Foldable(fold))
import Data.Monoid (getSum, Sum(Sum))

type Input = [([String], [String])]

parseInput :: String -> Input
parseInput = map parse . lines
 where
  parse s =
    let [l, r] = splitOn " (contains " $ init s in (words l, splitOn ", " r)

allergenCandidates :: Input -> Map String [String]
allergenCandidates = intersect . foldr collect mempty
 where
  collect (ingredients, allergens) m =
    Map.unionWith (++) m $ Map.fromList $ map (, [ingredients]) allergens

  intersect = Map.map (foldr1 List.intersect)

part1 :: Input -> String
part1 input = show $ count undetermined allIngredients
 where
  allIngredients = foldMap fst input

  undetermined   = nub allIngredients \\ fold (allergenCandidates input)

  count elements inList =
    getSum $ foldMap (\e -> Sum (length $ List.elemIndices e inList)) elements


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
