{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.IntMap ((!?), IntMap)
import Data.List.Extra (splitOn)
import qualified Data.IntMap as Map
import Data.Either (rights)
import Control.Monad (join)
import Data.List (nub)
import Data.Foldable (find)
import Data.Bifunctor (Bifunctor(first))

data Input = Input { rules :: IntMap Rule
                   , messages :: [String]
                   } deriving Show

data Rule = C Char
          | R [Int]
          | R2 [Int] [Int]
          deriving Show

-- | A hand-rolled, very specific parser with an opinionated parser state.
--
-- For part2, we need to be able to return multiple matches/remaining strings
-- which complicates .. a lot.
newtype RuleP = RuleP
  { runRuleParser :: IntMap Rule -> String -> Either String [(String, String)] }

ruleP :: Rule -> RuleP
ruleP r = case r of
  C c    -> charP c
  R a    -> indexP a
  R2 a b -> indexP a `orElse` indexP b

charP :: Char -> RuleP
charP c = RuleP $ \_rs -> \case
  (x : xs)
    | x == c    -> pure [([c], xs)]
    | otherwise -> Left ("charP expected " ++ show c ++ " but got " ++ show x)
  [] -> Left ("charP expected " ++ show c ++ " but got nothing")

indexP :: [Int] -> RuleP
indexP [] = RuleP $ \_ s -> pure [([], s)]
indexP (i : is) = RuleP $ \rs s -> case rs !? i of
  Nothing -> Left ("no parse: indexP not found: " ++ show i)
  Just r  -> runRuleParser (combine (ruleP r) (indexP is)) rs s

combine :: RuleP -> RuleP -> RuleP
combine a b = RuleP $ \rs s -> do
  ras <- runRuleParser a rs s
  case rights $ map (runB rs) ras of
    []  -> Left $ "combine: right-hand did not parse any of " ++ show ras
    rbs -> pure $ join rbs
 where
  runB rs (ra, resta) = case runRuleParser b rs resta of
    Left  l -> Left l
    Right x -> Right $ map (first (ra ++)) x

orElse :: RuleP -> RuleP -> RuleP
orElse a b = RuleP $ \rs s -> case runRuleParser a rs s of
  Left  _  -> runRuleParser b rs s
  Right ra -> case runRuleParser b rs s of
    Left  _  -> pure ra
    Right rb -> pure $ nub $ ra ++ rb

parseInput :: String -> Input
parseInput s =
  let [rs, ms] = splitOn "\n\n" s
  in Input (Map.fromList $ map readRule $ lines rs) (lines ms)

readRule :: String -> (Int, Rule)
readRule s = let [i, rest] = splitOn ": " s in (read i, go rest)
 where
  go ('"' : c : '"' : _) = C c
  go s = case splitOn "|" s of
    [a] -> R (parse a)
    [a, b] -> R2 (parse a) (parse b)
    _   -> error "too many |"

  parse = map read . words

doneAfter :: RuleP -> RuleP
doneAfter p = RuleP $ \rs s -> case runRuleParser p rs s of
  Right rs -> case find (null . snd) rs of
    Nothing -> Left $ "unconsumed: " ++ show rs
    Just x  -> pure [x]
  Left x -> Left x

parseOnly :: RuleP -> IntMap Rule -> String -> Either String String
parseOnly p rs s = do
  res <- runRuleParser p rs s
  case find (null . snd) res of
    Nothing     -> Left $ "unconsumed: " ++ show rs
    Just (x, _) -> pure x

part1 :: Input -> String
part1 Input {..} = show . length . rights $ map parse messages
  where parse = parseOnly (indexP [0]) rules

part2 :: Input -> String
part2 i@Input {..} = show . length . rights $ map parse messages
  where parse = parseOnly (indexP [0]) $ rules' i

rules' :: Input -> IntMap Rule
rules' =
  Map.insert 8 (R2 [42] [42, 8])
    . Map.insert 11 (R2 [42, 31] [42, 11, 31])
    . rules

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
