module Main where

import           Data.List.Extra
import qualified Data.Set        as Set

part1 :: String -> String
part1 input =
  show
    $ length
    $ filter (isValid . split (\c -> c == ' ' || c == '\n'))
    $ splitOn "\n\n" input
 where
  isValid =
    Set.isSubsetOf requiredKeys
    . Set.fromList
    . map (take 3)

  requiredKeys = Set.fromList
    [ "byr"
    , "iyr"
    , "eyr"
    , "hgt"
    , "hcl"
    , "ecl"
    , "pid"
      -- , "cid"(Country ID)
    ]

part2 :: String -> String
part2 input = input

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
  -- putStrLn "part two"
  -- putStrLn $ part2 input
