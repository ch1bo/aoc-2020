{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Data.IntMap ((!), (!?), IntMap)
import Data.List.Extra (isPrefixOf, splitOn)
import qualified Data.IntMap as Map
import Data.Either (rights)
import Debug.Trace (trace)
import Data.Tuple.Extra (first)

data Input = Input { rules :: IntMap Rule
                   , messages :: [String]
                   } deriving Show

data Rule = C Char
          | R [Int]
          | R2 [Int] [Int]
          deriving Show

-- | A hand-rolled, very specific parser with an opinionated parser state.
newtype RuleP = RuleP
  { runRuleParser :: IntMap Rule -> String -> Either (String, String) (String, String) }

ruleP :: Rule -> RuleP
ruleP r = case r of
  C c -> charP c
  R a -> indexP a
  R2 a b ->
    let (prefix, a', b') = commonPrefix a b
    in
      combine (indexP prefix) $ if length a' >= length b'
        then indexP a' `orElse` indexP b'
        else indexP b' `orElse` indexP a'

commonPrefix :: Eq a => [a] -> [a] -> ([a], [a], [a])
commonPrefix [] b  = ([], [], b)
commonPrefix a  [] = ([], a, [])
commonPrefix (a : as) (b : bs)
  | a == b    = let (p, x, y) = commonPrefix as bs in (a : p, x, y)
  | otherwise = ([], a : as, b : bs)

charP :: Char -> RuleP
charP c = RuleP $ \_rs -> \case
  (x : xs)
    | x == c -> pure ([c], xs)
    | otherwise -> Left
      ("charP expected " ++ show c ++ " but got " ++ show x, x : xs)
  [] -> Left ("charP expected " ++ show c ++ " but got nothing", [])

indexP :: [Int] -> RuleP
indexP [] = RuleP $ \_ s -> pure ([], s)
indexP (i : is) = RuleP $ \rs s -> case rs !? i of
  Nothing -> Left ("no parse: indexP not found: " ++ show i, s)
  Just r  -> runRuleParser
    (combine (label (show i ++ ": " ++ show r) $ ruleP r) (indexP is))
    rs
    s

combine :: RuleP -> RuleP -> RuleP
combine a b = RuleP $ \rs s -> do
  (ra, resta) <- runRuleParser a rs s
  (rb, restb) <- runRuleParser b rs resta
  pure (ra ++ rb, restb)

orElse :: RuleP -> RuleP -> RuleP
orElse a b = RuleP $ \rs s -> case runRuleParser a rs s of
  Right ([], rest) -> runRuleParser b rs rest
  Right x -> pure x
  Left  _ -> runRuleParser b rs s

label :: String -> RuleP -> RuleP
label l p = RuleP $ \rs s ->
  case trace (l ++ " ? " ++ show s) runRuleParser p rs s of
    Right (x, rest) ->
      trace (l ++ " > " ++ show x ++ ", " ++ show rest) pure (x, rest)
    Left (e, rest) -> trace (l ++ " X " ++ show rest) Left (l ++ e, rest)

doneAfter :: RuleP -> RuleP
doneAfter p = RuleP $ \rs s -> case runRuleParser p rs s of
  Right (r, []) -> pure (r, [])
  Right (_, rest) -> Left ("unconsumed", rest)
  Left  x -> Left x

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
part1 Input {..} = show . length . rights $ map parse messages
  where parse = runRuleParser (doneAfter $ ruleP $ R [0]) rules

part2 :: Input -> String
part2 i = unlines . map show $ map parse $ messages i
  where parse = runRuleParser (doneAfter $ ruleP $ R [0]) $ rules' i

rules' :: Input -> IntMap Rule
rules' =
  Map.insert 8 (R2 [42] [42, 8])
    . Map.insert 11 (R2 [42, 31] [42, 11, 31])
    . rules

main :: IO ()
main = do
  -- putStrLn "Part one (test):"
  -- putStrLn $ part1 test
  -- putStrLn "Part one (input):"
  -- putStrLn $ part1 input
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  -- putStrLn "Part two (input):"
  -- putStrLn $ part2 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
