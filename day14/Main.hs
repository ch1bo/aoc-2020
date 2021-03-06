{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (forM_, gets, modify, execState)
import Data.Bits
import Data.Tuple.Extra (first, second)

type Input = [Statement]

data MaskedBit = BitX | Bit1 | Bit0
               deriving Show

data Statement = Mask [MaskedBit]
               | Mem Natural Natural
               deriving Show

parseInput :: String -> Input
parseInput = either (error . show) id . parse statements ""
 where
  statements :: Parsec Void String Input
  statements = many statement

  statement  = try mask <|> try mem

  mask =
    string "mask" *> space *> char '=' *> space *> (Mask <$> many bit) <* space

  bit = BitX <$ char 'X' <|> Bit1 <$ char '1' <|> Bit0 <$ char '0'

  mem = do
    string "mem"
    index <- read <$> between (char '[') (char ']') decimal
    val   <- read <$> (space *> char '=' *> space *> decimal)
    Mem index val <$ space

  decimal = some digitChar <* space

maskValue :: [MaskedBit] -> Natural -> Natural
maskValue mask n = foldr apply n
  $ zip mask [(length mask - 1), (length mask - 2) .. 0]
 where
  apply (b, i) n = case b of
    BitX -> n
    Bit1 -> n `setBit` i
    Bit0 -> n `clearBit` i

type Memory = Map Natural Natural

execute :: Input -> Memory
execute ss = fst $ execState (mapM_ go ss) (Map.empty, replicate 36 BitX)
 where
  go = \case
    Mask bits -> modify . second $ const bits
    Mem idx val ->
      modify $ \(mem, mask) -> (Map.insert idx (maskValue mask val) mem, mask)

part1 :: Input -> String
part1 = show . sum . Map.elems . execute

-- | Mask and expand addresses considering floating bits.
maskAddress :: [MaskedBit] -> Natural -> [Natural]
maskAddress mask a = apply a
  $ zip mask [(length mask - 1), (length mask - 2) .. 0]
 where
  apply a [] = [a]
  apply a ((b, i) : bs) = case b of
    BitX -> apply (a `clearBit` i) bs <> apply (a `setBit` i) bs
    Bit0 -> apply a bs
    Bit1 -> apply (a `setBit` i) bs

execute2 :: Input -> Memory
execute2 ss = fst $ execState (mapM_ go ss) (Map.empty, replicate 36 BitX)
 where
  go = \case
    Mask bits   -> modify . second $ const bits
    Mem idx val -> do
      mask <- gets snd
      let as = maskAddress mask idx
      forM_ as $ \a -> modify $ first (Map.insert a val)

part2 :: Input -> String
part2 = show . sum . Map.elems . execute2

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
