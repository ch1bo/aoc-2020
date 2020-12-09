{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read (Read(readPrec), Lexeme(Ident, Symbol), lexP)
import Safe (atMay)
import Control.Monad.State (gets, evalState, modify, get)
import Data.Bifunctor (second, Bifunctor(first))
import Control.Applicative (Alternative((<|>)))
import Data.Foldable (find)

type Input = Program

parseInput :: String -> Input
parseInput = map read . lines

type Program = [Instruction]

data Instruction = Nop Int
                 | Acc Int
                 | Jmp Int
                 deriving Show

instance Read Instruction where
  readPrec = lexP >>= \case
    Ident s
      | s == "nop" -> Nop <$> readParam
      | s == "acc" -> Acc <$> readParam
      | s == "jmp" -> Jmp <$> readParam
    x -> error $ "read Instruction: no parse " <> show x
   where
    readParam = lexP >>= \case
      Symbol s
        | s == "+" -> readPrec
        | s == "-" -> negate <$> readPrec
      e -> error (show e)


data Result = InfiniteLoop Int -- ^ Value of acc just before looping
            | InvalidJmp Int -- ^ Value of acc just beore the jump
            | Terminated Int -- ^ Value of acc just before terminating
            deriving Show

runProgram :: Program -> Result
runProgram p = evalState (go 0) ([], 0)
 where
  go i
    | i == length p = gets (Terminated . snd)
    | otherwise = do
      (hist, acc) <- get
      if i `elem` hist
        then pure $ InfiniteLoop acc
        else case p `atMay` i of
          Nothing -> pure $ InvalidJmp acc
          Just (Nop _) -> step i >> go (i + 1)
          Just (Acc x) -> step i >> accumulate x >> go (i + 1)
          Just (Jmp j) -> do
            if i + j == length p
              then pure $ Terminated acc
              else step i >> go (i + j)

  step i = modify (first (i :))

  accumulate x = modify (second (+ x))

part1 :: Input -> String
part1 = show . runProgram

-- REVIEW quite redundant -> refactor?
permuteNopJmp :: Program -> [Program]
permuteNopJmp [] = []
permuteNopJmp [i] = case i of
  Nop x -> [[Nop x], [Jmp x]]
  Acc _ -> [[i]]
  Jmp x -> [[Nop x], [Jmp x]]
permuteNopJmp (i : is) = case i of
  Nop x -> [Jmp x : is] <|> (i :) <$> permuteNopJmp is
  Acc _ -> (i :) <$> permuteNopJmp is
  Jmp x -> [Nop x : is] <|> (i :) <$> permuteNopJmp is

part2 :: Input -> String
part2 =
  show . find (terminated . snd) . map (\p -> (p, runProgram p)) . permuteNopJmp
 where
  terminated (Terminated _) = True
  terminated _ = False

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
