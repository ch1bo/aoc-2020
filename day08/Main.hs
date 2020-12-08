{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Text.Read

type Input = [Instruction]

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

parseInput :: String -> Input
parseInput = map read . lines

part1 :: Input -> String
part1 = unlines . map show

part2 :: Input -> String
part2 = undefined

main :: IO ()
main = do
  putStrLn "Part one (test):"
  putStrLn $ part1 test
  putStrLn "Part two (test):"
  putStrLn $ part2 test
  putStrLn "Part one (input):"
  putStrLn $ part1 input
  putStrLn "Part two (input):"
  putStrLn $ part2 input

test :: Input
test = unsafePerformIO $ parseInput <$> readFile "test"
{-# NOINLINE test #-}

input :: Input
input = unsafePerformIO $ parseInput <$> readFile "input"
{-# NOINLINE input #-}
