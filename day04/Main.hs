module Main where

import qualified Data.Set        as Set
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec
import qualified Data.Map as Map
import Control.Monad (void)
import Data.Char (isAlphaNum, isSymbol)
import Debug.Trace (trace)
import Data.Map ((!?))
import Data.Maybe (catMaybes)

data Passport = Passport
  { byr :: String
  , iyr :: String
  , eyr :: String
  , hgt :: String
  , hcl :: String
  , ecl :: String
  , pid :: String
  , cid :: Maybe String
  } deriving Show

passport :: Parser (Maybe Passport)
passport = do
  fs <- Map.fromList <$> field `sepBy` char ' '
  pure $ Passport
    <$> fs !? "byr"
    <*> fs !? "iyr"
    <*> fs !? "eyr"
    <*> fs !? "hgt"
    <*> fs !? "hcl"
    <*> fs !? "ecl"
    <*> fs !? "pid"
    <*> pure (fs !? "cid")
  <?> "maybe passport"

field :: Parser (String,String)
field = do
  k <- many1 alphaNum <* char ':'
  v <- many1 (satisfy (\x -> x == '#' || isAlphaNum x))
  pure (k,v)
  <?> "field"

part1 :: String -> String
part1 input = do
  case parse (passport `sepBy` newline) "input" $ inline input of
    Left e -> error (show e)
    Right ps -> show . length $ catMaybes ps
 where
  inline ('\n':'\n':xs) = '\n':inline xs
  inline ('\n':x:xs) = ' ':inline (x:xs)
  inline (x:xs) = x:inline xs
  inline [] = []

part2 :: String -> String
part2 input = input

main :: IO ()
main = do
  let input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
              \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
              \\n\
              \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
              \hcl:#cfa07d byr:1929\n\
              \\n\
              \hcl:#ae17e1 iyr:2013\n\
              \eyr:2024\n\
              \ecl:brn pid:760753108 byr:1931\n\
              \hgt:179cm\n\
              \\n\
              \hcl:#cfa07d eyr:2025 pid:166559648\n\
              \iyr:2011 ecl:brn hgt:59in\n"
  -- input <- readFile "input"
  putStrLn "part one"
  putStrLn $ part1 input
  -- putStrLn "part two"
  -- putStrLn $ part2 input
