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
import Data.Foldable (find)
import Safe (headMay)

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

allIngredients :: Input -> [String]
allIngredients = foldMap fst

inertIngredients :: Input -> [String]
inertIngredients input =
  nub (allIngredients input) \\ fold (allergenCandidates input)

part1 :: Input -> String
part1 input = show $ count (inertIngredients input) (allIngredients input)
 where
  count elements inList =
    getSum $ foldMap (\e -> Sum (length $ List.elemIndices e inList)) elements

labelIngredients :: Input -> Map String String
labelIngredients = go mempty . allergenCandidates
 where
  go labels input
    | Map.null input = labels
    | otherwise = case findSingleton input of
      Nothing -> error $ "can't simplify: " ++ show input
      Just (allergen, ingredient) -> go
        (labels <> Map.singleton allergen ingredient)
        (Map.map (List.delete ingredient) $ Map.delete allergen input)

findSingleton :: Map a [b] -> Maybe (a, b)
findSingleton m = case find ((1 ==) . length . snd) $ Map.toList m of
  Just (k, [v]) -> Just (k, v)
  _ -> Nothing

part2 :: Input -> String
part2 input =
  List.intercalate ","
    $ map snd
    $ List.sortOn fst
    $ Map.toList
    $ labelIngredients input

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
