module Main where

import Text.Printf (printf)
import Data.List.Extra (splitOn, breakOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (Bifunctor(second))
import GHC.IO (unsafePerformIO)

type Input = Map String [(Int, String)]

parseInput :: String -> Input
parseInput s = foldMap parseLine $ lines s
 where
  parseLine l = do
    let (h : cs) = splitOn " bags contain " l
    Map.singleton h $ parseContent (head cs)

  parseContent c
    | c == "no other bags." = []
    | otherwise = map parseBag $ splitOn ", " c

  parseBag b =
    let (numString, rest) = breakOn " " b
    in (read numString, head . splitOn " bag" $ tail rest)

data Tree = Leaf String
          | Tree String [(Int, Tree)]
          deriving Show

rootName :: Tree -> String
rootName (Leaf s) = s
rootName (Tree s _) = s

toForest :: Input -> [Tree]
toForest m = map toTree $ Map.keys m
 where
  toTree :: String -> Tree
  toTree s = case Map.lookup s m of
    Nothing -> Leaf s
    Just [] -> Leaf s
    Just cs -> Tree s (map (second toTree) cs)

containsBag :: String -> Tree -> Bool
containsBag s (Leaf n) = s == n
containsBag s (Tree n cs)
  | s == n = True
  | otherwise = any (containsBag s . snd) cs

countBagsIn :: Tree -> Int
countBagsIn (Leaf _) = 0
countBagsIn (Tree _ cs) = foldr go 0 cs
  where go (i, t) s = s + i * (countBagsIn t + 1)

part1 :: Input -> String
part1 =
  show
    . pred -- -1 because "shiny gold" is also in the result
    . length
    . filter (containsBag "shiny gold")
    . toForest

part2 :: Input -> String
part2 =
  show
    . maybe (error "missing a bag!") countBagsIn
    . findTree "shiny gold"
    . toForest
 where
  findTree _ [] = Nothing
  findTree s (t : ts)
    | rootName t == s = Just t
    | otherwise = findTree s ts

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 test
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  putStrLn "Part one (input):"
  putStrLn $ part1 input
  putStrLn "Part two (input):"
  putStrLn $ part2 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
