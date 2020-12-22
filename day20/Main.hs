{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Data.List.Extra (replace, intercalate, nub, delete, splitOn)
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (foldl')
import Data.List (transpose)

data Tile = Tile
  { tileId :: Natural
  , tileData :: [String]
  } deriving Eq

instance Show Tile where
  show (Tile tid d) = unlines (header : d)
    where header = "Tile " ++ show tid ++ ":"

readTile :: String -> Tile
readTile s = case lines s of
  [] -> error "readTile empty"
  (x : xs) -> Tile (read $ init $ words x !! 1) xs

-- rotate :: Tile -> Tile
-- rotate (Tile tid d) = flipH . Tile tid $ transpose d

-- trans :: Tile -> Tile
-- trans (Tile tid d) = Tile tid $ transpose d

-- flipH :: Tile -> Tile
-- flipH (Tile tid d) = Tile tid $ map reverse d

-- flipV :: Tile -> Tile
-- flipV (Tile tid d) = Tile tid $ reverse d

getT :: Natural -> Input -> Tile
getT x ts = let (Just t) = find ((== x) . tileId) ts in t

top :: Tile -> String
top = head . tileData

bottom :: Tile -> String
bottom = last . tileData

left :: Tile -> String
left = map head . tileData

right :: Tile -> String
right = map last . tileData

borders :: Tile -> [String]
borders t = [top t, bottom t, left t, right t]

-- | Find corners in a list of tiles. Each tile which has only two matching
-- borders is a corner.
corners :: [Tile] -> [Tile]
corners ts = filter ((== 2) . length . matchingBorders) ts
 where
  -- A border matches if it or it's reverse is equal
  matchingBorders t =
    filter (\b -> any (matches b) (otherBorders t)) $ borders t

  otherBorders = nub . concatMap borders . otherTiles

  otherTiles t = delete t ts

  matches a b = a == b || reverse a == b

-- | Arrange tiles to a picture by
-- * Find a corner
-- * Flip it such that it is the top-left corner
-- * Find matching tiles in row until another corner is found (and modify them
--   if match is reversed)
-- * Find tiles matching the bottom of the first row
-- * Continue until no tiles left
arrange :: [Tile] -> [[Tile]]
arrange ts = [corners ts]

display :: [[Tile]] -> String
display = unlines . map displayRow
 where
  displayRow ts = merge $ map show ts

  merge = unlines . map unwords . transpose . map lines

type Input = [Tile]

parseInput :: String -> Input
parseInput = map readTile . splitOn "\n\n"

part1 :: Input -> String
part1 = show . product . map tileId . corners

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
