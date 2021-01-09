{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List (elemIndex)

type PubKey = Integer

data Input = Input
  { cardPub :: PubKey
  , doorPub :: PubKey
  }
  deriving Show

loops :: Integer -> [Integer]
loops n = iterate go n where go v = v * n `rem` 20201227

transform :: Integer -> Int -> Integer
transform sn ls = loops sn !! ls

part1 :: Input -> String
part1 Input {..} = show encryptionKey
 where
  cardLoopSize  = elemIndex cardPub $ loops 7

  encryptionKey = transform doorPub <$> cardLoopSize

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
test = Input 5764801 17807724

input :: Input
input = Input 10604480 4126658
