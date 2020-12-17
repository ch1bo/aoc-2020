{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (mapMaybe)
import Data.Foldable (find)

type Range = (Natural, Natural)

type Ticket = [Natural]

data Input = Input { rules :: Map String (Range, Range)
                   , myTicket :: Ticket
                   , nearbyTickets :: [Ticket]
                   }
             deriving Show

parseInput :: String -> Input
parseInput = either (error . errorBundlePretty) id . parse parseInput ""
 where
  parseInput :: Parsec Void String Input
  parseInput = do
    rs <- some (rule <* newline)
    newline *> string "your ticket:" *> newline
    my <- ticket <* newline
    newline *> string "nearby tickets:" <* newline
    nearby <- some (ticket <* newline)
    pure $ Input (Map.fromList rs) my nearby

  rule = try $ do
    label <- takeWhile1P Nothing (/= ':') <* string ": "
    r1    <- range <* string " or "
    r2    <- range
    pure (label, (r1, r2))

  range  = (,) <$> decimal <* char '-' <*> decimal

  ticket = decimal `sepBy` char ','

inRange :: Range -> Natural -> Bool
inRange (l, h) i = l <= i && i <= h

valid :: Natural -> (Range, Range) -> Bool
valid i (lower, upper) = inRange lower i || inRange upper i

-- | Validate and return fist invalid value
validateTicket :: [(Range, Range)] -> Ticket -> Maybe Natural
validateTicket rs = find (\t -> not $ any (valid t) rs)

part1 :: Input -> String
part1 Input {..} =
  show $ sum $ mapMaybe (validateTicket $ Map.elems rules) nearbyTickets

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
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
