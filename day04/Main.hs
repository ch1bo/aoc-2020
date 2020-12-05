{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (take, takeWhile)
import qualified Data.Map as Map
import Data.Char (isHexDigit, isAlphaNum)
import GHC.TypeLits (natVal, Nat, KnownNat)
import Data.Proxy (Proxy(..))
import Data.Attoparsec.Text
import Control.Monad (replicateM, unless)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Either (rights)

newtype Range (l :: Nat) (h :: Nat) = Range { unsafeRange :: Integer }
                                    deriving Show

parseRange :: (KnownNat l, KnownNat h) => proxy l -> proxy h -> Parser (Range l h)
parseRange l h = do
  i <- decimal
  unless (i >= natVal l && i <= natVal h) $
    fail $ show i ++ " not in range [" ++ show (natVal l) ++ "," ++ show (natVal h) ++ "]"
  pure (Range i)

data Height
  = Metric (Range 150 193) -- ^ cm
  | Imperial (Range 59 76) -- ^ in
  deriving Show

parseHeight :: Parser Height
parseHeight =
  imperial <|> metric
 where
  imperial = Imperial <$> parseRange (Proxy @59) (Proxy @76) <* string "in" <?> "imperial height"
  
  metric = Metric <$> parseRange (Proxy @150) (Proxy @193) <* string "cm" <?> "metric height"

parseHairColor :: Parser Text
parseHairColor =
  fmap Text.pack . (:) <$> char '#' <*> exactly 6 (satisfy isHexDigit)
  <?> "hair color"

exactly :: Int -> Parser a -> Parser [a]
exactly = replicateM

data Passport = Passport
  { byr :: Range 1920 2002
  , iyr :: Range 2010 2020
  , eyr :: Range 2020 2030
  , hgt :: Height
  , hcl :: Text
  , ecl :: Text
  , pid :: Text
  , cid :: Maybe Text
  } deriving Show

passport :: Parser Passport
passport = do
  fs <- Map.fromList <$> field `sepBy` char ' '
  Passport
    <$> runP fs "byr" (parseRange (Proxy @1920) (Proxy @2002))
    <*> runP fs "iyr" (parseRange (Proxy @2010) (Proxy @2020))
    <*> runP fs "eyr" (parseRange (Proxy @2020) (Proxy @2030))
    <*> runP fs "hgt" parseHeight
    <*> runP fs "hcl" parseHairColor
    <*> runP fs "ecl" (choice $ map string ["amb","blu","brn","gry","grn","hzl","oth"])
    <*> runP fs "pid" (Text.pack <$> exactly 9 digit)
    <*> pure (fs !? "cid")
  <?> "passport"
 where
  runP m k p = m !? k >>= either fail pure . parseOnly (p <* endOfInput <?> k)

  (!?) m k = maybe (fail $ "missing field: " ++ k) pure $ Map.lookup k m

field :: Parser (String,Text)
field = do
  k <- many1 (satisfy isAlphaNum) <* char ':'
  v <- many1 (satisfy (\x -> x == '#' || isAlphaNum x))
  pure (k, Text.pack v)
  <?> "field"

parsePassports :: String -> [Either String Passport]
parsePassports =
  map (parseOnly passport . Text.pack) . lines . inline
 where
  inline ('\n':'\n':xs) = '\n':inline xs
  inline ('\n':x:xs) = ' ':inline (x:xs)
  inline (x:xs) = x:inline xs
  inline [] = []

part1 :: String -> String
part1 = show . length . rights . parsePassports

part2 :: String -> String
part2 = part1 -- unlines . map show . parsePassports

main :: IO ()
main = do
  -- let input = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
  --             \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
  --             \\n\
  --             \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
  --             \hcl:#cfa07d byr:1929\n\
  --             \\n\
  --             \hcl:#ae17e1 iyr:2013\n\
  --             \eyr:2024\n\
  --             \ecl:brn pid:760753108 byr:1931\n\
  --             \hgt:179cm\n\
  --             \\n\
  --             \hcl:#cfa07d eyr:2025 pid:166559648\n\
  --             \iyr:2011 ecl:brn hgt:59in\n"
  input <- readFile "input"
  putStrLn "part one"
  putStrLn $ part1 input
  putStrLn "part two"
  putStrLn $ part2 input
