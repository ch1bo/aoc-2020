{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Text.Megaparsec
  (parse, errorBundlePretty, sepBy, some, Parsec, MonadParsec(takeWhile1P, try))
import Text.Megaparsec.Char (string, char, newline)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Data.Maybe (mapMaybe)
import Data.List (find, isPrefixOf, delete)

type Range = (Natural, Natural)

data Rule = Rule { ruleName :: String
                 , ruleLower :: Range
                 , ruleUpper :: Range
                 }
          deriving (Eq, Show)

type Ticket = [Natural]

data Input = Input { rules :: [Rule]
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
    pure $ Input rs my nearby

  rule = try $ do
    label <- takeWhile1P Nothing (/= ':') <* string ": "
    r1    <- range <* string " or "
    r2    <- range
    pure $ Rule label r1 r2

  range  = (,) <$> decimal <* char '-' <*> decimal

  ticket = decimal `sepBy` char ','

inRange :: Range -> Natural -> Bool
inRange (l, h) i = l <= i && i <= h

valid :: Natural -> Rule -> Bool
valid i (Rule _ lower upper) = inRange lower i || inRange upper i

-- | Validate and return fist invalid value
validateTicket :: [Rule] -> Ticket -> Maybe Natural
validateTicket rs = find (\t -> not $ any (valid t) rs)

part1 :: Input -> String
part1 Input {..} = show $ sum $ mapMaybe (validateTicket rules) nearbyTickets

validTickets :: [Rule] -> [Ticket] -> [Ticket]
validTickets rs = filter ((== Nothing) . validateTicket rs)

inferFields :: [Ticket] -> [Rule] -> [(Int, String)]
inferFields _  [] = [] -- done
inferFields [] _  = error "ran out of tickets"
inferFields tickets remainingRules = infer 0 remainingRules
 where
  infer i _ | i >= length (head tickets) = []
  infer i rs = case matchingRules rs $ map (!! i) tickets of
    [r] -> (i, ruleName r) : infer 0 (delete r rs)
    _   -> infer (i + 1) rs

  matchingRules rs vs = filter (\r -> all (`valid` r) vs) rs

part2 :: Input -> String
part2 Input {..} =
  show . product . map (\(i, _) -> myTicket !! i) $ departureFields
 where
  tickets = validTickets rules nearbyTickets

  fields  = inferFields tickets rules

  departureFields = filter (\(_, s) -> "departure" `isPrefixOf` s) fields

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
