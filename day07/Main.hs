module Main where

import Text.Printf (printf)
import Data.List.Extra (splitOn, breakOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor (Bifunctor(second))

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

toForest :: Input -> [Tree]
toForest m = map toTree $ Map.keys m
 where
  toTree :: String -> Tree
  toTree s = case Map.lookup s m of
    Nothing -> Leaf s
    Just cs -> Tree s (map (second toTree) cs)

containsBag :: String -> Tree -> Bool
containsBag s (Leaf n) | s == n = True
containsBag s (Tree n cs)
  | s == n = True
  | otherwise = any (containsBag s . snd) cs

part1 :: Input -> String
part1 =
  show
    . pred -- -1 because "shiny gold" is also in the result
    . length
    . filter (containsBag "shiny gold")
    . toForest
  -- unlines . map (\t -> show (containsBag "shiny gold" t, t)) . toForest

part2 :: Input -> String
part2 = undefined

main :: IO ()
main = do
  go "test"
  go "input"
 where
  go fn = do
    testInput <- parseInput <$> readFile fn
    printf "Part one (%s):\n" fn
    putStrLn $ part1 testInput
    -- printf "Part two (%s):\n" fn
    -- print $ part2 testInput
