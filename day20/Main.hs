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
import Data.Map (Map)
import Debug.Trace (trace)

data Tile = Tile
  { tileId :: Natural
  , tileData :: [String]
  }
instance Eq Tile where
  a == b = tileId a == tileId b

instance Show Tile where
  show (Tile tid d) = unlines (header : d)
    where header = "Tile " ++ show tid ++ ":"

readTile :: String -> Tile
readTile s = case lines s of
  [] -> error "readTile empty"
  (x : xs) -> Tile (read $ init $ words x !! 1) xs

-- rotate :: Tile -> Tile
-- rotate (Tile tid d) = flipH . Tile tid $ transpose d

trans :: Tile -> Tile
trans (Tile tid d) = Tile tid $ transpose d

flipH :: Tile -> Tile
flipH (Tile tid d) = Tile tid $ map reverse d

flipV :: Tile -> Tile
flipV (Tile tid d) = Tile tid $ reverse d

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
arrange ts = firstRow : rows firstRow
 where
  firstRow = topLeftRotated : row topLeftRotated

  rows [] = []
  rows (t : _) = case below t of
    Nothing -> []
    Just r ->
      let
        first = nothingLeft r
        ts    = first : row first
      in ts : rows ts

  row t = case rightOf t of
    Nothing -> []
    Just r  -> r : row r

  rightOf t = case lookupB BR t of
    Just (BT, r) -> Just $ trans r
    Just (BB, r) -> Just $ flipH $ trans r
    Just (BL, r) -> Just r
    Just (BR, r) -> Just $ flipH r
    Nothing -> Nothing

  below t = case lookupB BB t of
    Just (BT, r) -> Just r
    Just (BB, r) -> Just $ flipV r
    Just (BL, r) -> Just $ trans r
    Just (BR, r) -> Just $ trans $ flipH r
    Nothing -> Nothing

  nothingLeft t = case lookupB BL t of
    Just _  -> flipH t
    Nothing -> t

  nothingAbove t = case lookupB BT t of
    Just _  -> flipV t
    Nothing -> t

  topLeft = head $ corners ts -- TODO partial

  topLeftRotated = nothingLeft $ nothingAbove topLeft

  bm t = foldMap borderMap (delete t ts)

  lookupB b t = case b of
    BT -> lookup' (top t) $ bm t
    BB -> lookup' (bottom t) $ bm t
    BL -> lookup' (left t) $ bm t
    BR -> lookup' (right t) $ bm t

  lookup' s m = case Map.lookup s m of
    Nothing -> mirrorResult <$> Map.lookup (reverse s) m
    Just x  -> Just x

  mirrorResult (BT, t) = (BT, flipH t)
  mirrorResult (BB, t) = (BB, flipH t)
  mirrorResult (BL, t) = (BL, flipV t)
  mirrorResult (BR, t) = (BR, flipV t)

borderMap :: Tile -> Map String (Border, Tile)
borderMap t = Map.fromList
  [(top t, (BT, t)), (bottom t, (BB, t)), (left t, (BL, t)), (right t, (BR, t))]

data Border = BT
            | BB
            | BL
            | BR
            deriving (Eq, Ord, Show)

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
