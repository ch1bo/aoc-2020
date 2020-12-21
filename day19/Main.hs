{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.IntMap ((!), (!?), IntMap)
import Data.List.Extra (splitOn)
import qualified Data.IntMap as Map
import Data.Either (rights)

data Input = Input { rules :: IntMap Rule
                   , messages :: [String]
                   } deriving Show

data Rule = C Char
          | R [Int]
          | R2 [Int] [Int]
          deriving Show

-- | A hand-rolled, very specific parser with an opinionated parser state.
newtype RuleP = RuleP
  { runRuleParser :: IntMap Rule -> String -> Either String (String, String) }

ruleP :: Rule -> RuleP
ruleP r = case r of
  C c    -> charP c
  R a    -> indexP a
  R2 a b -> indexP a `orElse` indexP b

charP :: Char -> RuleP
charP c = RuleP $ \_rs -> \case
  (x : xs) | x == c -> pure ([c], xs)
  _ -> Left $ "no parse: charP " ++ show c

indexP :: [Int] -> RuleP
indexP [] = RuleP $ \_ s -> pure ([], s)
indexP (i : is) = RuleP $ \rs s -> case rs !? i of
  Nothing -> Left $ "no parse: indexP not found: " ++ show i
  Just r  -> runRuleParser (combine (ruleP r) (indexP is)) rs s

combine :: RuleP -> RuleP -> RuleP
combine a b = RuleP $ \rs s -> do
  (ra, resta) <- runRuleParser a rs s
  (rb, restb) <- runRuleParser b rs resta
  pure (ra ++ rb, restb)

orElse :: RuleP -> RuleP -> RuleP
orElse a b = RuleP $ \rs s -> case runRuleParser a rs s of
  Right x -> pure x
  Left  _ -> runRuleParser b rs s

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

part1 :: Input -> String
part1 Input {..} = show . length . rights $ map parseOnly messages
 where
  parse = runRuleParser (ruleP rule0) rules
  parseOnly s = parse s >>= \case
    (x, []  ) -> pure x
    (_, rest) -> Left $ "unconsumed: " ++ rest
  rule0 = rules ! 0

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
