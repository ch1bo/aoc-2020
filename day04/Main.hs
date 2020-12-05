{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec
import qualified Data.Map as Map
import Data.Map ((!?))
import Data.Maybe (catMaybes)
import Data.Char (isAlphaNum)
import GHC.TypeLits (natVal, Nat, KnownNat)
import Data.Proxy (Proxy(..))

newtype Range (l :: Nat) (h :: Nat) = Range { unsafeRange :: Integer }
                                    deriving Show

parseRange :: (KnownNat l, KnownNat h) => proxy l -> proxy h -> String -> Maybe (Range l h)
parseRange l h s
  | i >= natVal l && i <= natVal h = Just (Range i)
  | otherwise = Nothing
 where
  i = read s -- REVIEW use a proper parser here as well?

data Height
  = Metric (Range 150 193) -- ^ cm
  | Imperial (Range 59 76) -- ^ in
  deriving Show

parseHeight :: String -> Maybe Height
parseHeight _ = Nothing

data Passport = Passport
  { byr :: Range 1920 2002
  , iyr :: Range 2010 2020
  , eyr :: Range 2020 2030
  , hgt :: Height
  , hcl :: String
  , ecl :: String
  , pid :: String
  , cid :: Maybe String
  } deriving Show

passport :: Parser (Maybe Passport)
passport = do
  fs <- Map.fromList <$> field `sepBy` char ' '
  pure $ Passport
    <$> (fs !? "byr" >>= parseRange (Proxy @1920) (Proxy @2002))
    <*> (fs !? "iyr" >>= parseRange (Proxy @2010) (Proxy @2020))
    <*> (fs !? "eyr" >>= parseRange (Proxy @2020) (Proxy @2030))
    <*> (fs !? "hgt" >>= parseHeight)
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
part2 = part1

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
  putStrLn "part two"
  putStrLn $ part2 input
