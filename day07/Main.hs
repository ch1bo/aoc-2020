module Main where

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

data Bag = Bag { name :: String, contents :: [(Int, Bag)] }
          deriving Show

toBags :: Input -> [Bag]
toBags m = map toBag $ Map.keys m
 where
  toBag :: String -> Bag
  toBag s = Bag
    { name = s
    , contents = maybe [] (map (second toBag)) $ Map.lookup s m
    }

containsBag :: String -> Bag -> Bool
containsBag s (Bag n cs)
  | s == n = True
  | otherwise = any (containsBag s . snd) cs

countBagsIn :: Bag -> Int
countBagsIn (Bag _ cs) = foldr go 0 cs
  where go (i, t) s = s + i * (countBagsIn t + 1)

part1 :: Input -> String
part1 =
  show
    . pred -- -1 because "shiny gold" is also in the result
    . length
    . filter (containsBag "shiny gold")
    . toBags

part2 :: Input -> String
part2 =
  show
    . maybe (error "missing a bag!") countBagsIn
    . findBag "shiny gold"
    . toBags
 where
  findBag _ [] = Nothing
  findBag s (t : ts)
    | name t == s = Just t
    | otherwise = findBag s ts

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
