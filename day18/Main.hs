{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)

data Exp = Add Exp Exp
         | Mul Exp Exp
         | Val Natural
         deriving Show

type Input = [Exp]

type Parser = Parsec Void String

-- | Simple expression parser which has no operator precedence and all operators
-- are left-associative.
--
-- NOTE: There is parser-combinators Control.Monad.Combinators.Expr to do this
-- properly
expP :: Parser Exp
expP = do
  term <- termP
  opP term <|> pure term
 where
  valP  = Val <$> natural

  termP = parens expP <|> valP

  opP a = addP a <|> mulP a

  addP a = try $ do
    symbol "+"
    r <- Add a <$> termP
    opP r <|> pure r

  mulP a = try $ do
    symbol "*"
    r <- Mul a <$> termP
    opP r <|> pure r

  natural = L.lexeme sc L.decimal

  symbol = L.symbol sc

  parens = between (symbol "(") (symbol ")")

  sc     = L.space hspace1 empty empty

parseInput :: String -> Input
parseInput =
  map (either (error . errorBundlePretty) id . parse lineP "") . lines
  where lineP = space *> expP <* eof

eval :: Exp -> Natural
eval = \case
  Val x   -> x
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y

part1 :: Input -> String
part1 = show . sum . map eval

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
