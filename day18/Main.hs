{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)

data Exp = Add Exp Exp
         | Mul Exp Exp
         | Val Natural
         deriving Show

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space hspace1 empty empty

natural :: Parser Natural
natural = L.lexeme sc L.decimal

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")


-- | Simple expression parser which has no operator precedence and all operators
-- are left-associative.
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

-- | Expression parser with precedences (addition binds higher than multiplication)
expPrecedenceP :: Parser Exp
expPrecedenceP = makeExprParser termP ops
 where
  valP  = Val <$> natural

  termP = parens expPrecedenceP <|> valP

  ops   = [[InfixL $ Add <$ symbol "+"], [InfixL $ Mul <$ symbol "*"]]

type Input = [String]

parseInput :: String -> Input
parseInput = lines

eval :: Exp -> Natural
eval = \case
  Val x   -> x
  Add x y -> eval x + eval y
  Mul x y -> eval x * eval y

parse' :: Parser a -> String -> a
parse' p = either (error . errorBundlePretty) id . parse (space *> p <* eof) ""

part1 :: Input -> String
part1 = show . sum . map (eval . parse' expP)

part2 :: Input -> String
part2 = show . sum . map (eval . parse' expPrecedenceP)

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
