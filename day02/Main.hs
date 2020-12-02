{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Text.Read
import Text.ParserCombinators.ReadP hiding (many, get)
import Control.Applicative (Alternative(many))

data Password = Password Policy String
              deriving (Eq, Show)

data Policy = Policy Int Int Char
            deriving (Eq, Show)

instance Read Password where
  readPrec = do
    (p :: Policy) <- readPrec
    lift $ char ':'
    lift skipSpaces
    s <- many get
    pure $ Password p s

instance Read Policy where
  readPrec = do
    low <- readPrec
    lift $ char '-'
    high <- readPrec
    lift skipSpaces
    c <- get
    pure $ Policy low high c

check1 :: Policy -> String -> Bool
check1 _ [] = False -- REVIEW
check1 (Policy low high c) xs =
  go 0 xs
 where
  go i [] -- at end
    | i >= low && i <= high = True
    | otherwise = False
  go i (x:xs) 
    | i > high = False -- abort
    | x == c = go (i+1) xs
    | otherwise = go i xs

check2 :: Policy -> String -> Bool
check2 _ [] = False -- REVIEW
check2 (Policy pos1 pos2 c) xs
  | length xs < pos1 = False
  | length xs < pos2 = False
  | otherwise = -- TODO XOR?
    xs !! (pos1-1) == c && xs !! (pos2-1) /= c ||
    xs !! (pos1-1) /= c && xs !! (pos2-1) == c

part1 :: IO ()
part1 = do
  putStrLn "part one"
  -- let input = "1-3 a: abcde\n\
  --             \1-3 b: cdefg\n\
  --             \2-9 c: ccccccccc\n"
  input <- readFile "input"
  print $ length . filter (\(Password p s) -> check1 p s) . map read $ lines input

part2 :: IO ()
part2 = do
  putStrLn "part two"
  -- let input = "1-3 a: abcde\n\
  --             \1-3 b: cdefg\n\
  --             \2-9 c: ccccccccc\n"
  input <- readFile "input"
  print $ length . filter (\(Password p s) -> check2 p s) . map read $ lines input

main :: IO ()
main = part1 >> part2
