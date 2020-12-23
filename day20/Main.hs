{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import System.IO.Unsafe (unsafePerformIO)
import Numeric.Natural (Natural)
import Data.List.Extra (replace, intercalate, nub, delete, splitOn)
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.List (foldl')
import Data.List (transpose)
import Data.Map (Map)
import Debug.Trace (trace)
import Data.List (uncons)
import Safe (headMay)

data Tile = Tile
  { tileId :: Natural
  , tileData :: [String]
  }
instance Eq Tile where
  a == b = tileId a == tileId b

instance Show Tile where
  show (Tile tid d) = unlines (header : d)
    where header = "Tile " ++ show tid ++ ":"

showTileRaw :: Tile -> String
showTileRaw = unlines . tail . init . map (tail . init) . tileData

readTile :: String -> Tile
readTile s = case lines s of
  [] -> error "readTile empty"
  (x : xs) -> Tile (read $ init $ words x !! 1) xs

trans :: Tile -> Tile
trans (Tile tid d) = Tile tid $ transpose d

flipH :: Tile -> Tile
flipH (Tile tid d) = Tile tid $ map reverse d

flipV :: Tile -> Tile
flipV (Tile tid d) = Tile tid $ reverse d

top :: Tile -> String
top = head . tileData

bottom :: Tile -> String
bottom = last . tileData

left :: Tile -> String
left = map head . tileData

right :: Tile -> String
right = map last . tileData

-- | Find corners in a list of tiles. Each tile which has only two matching
-- borders is a corner.
corners :: [Tile] -> [Tile]
corners ts = filter ((== 2) . length . matchingBorders) ts
 where
  -- A border matches if it or it's reverse is equal
  matchingBorders t =
    filter (\b -> isJust $ lookupBorder (bm t) b t) [BT, BB, BL, BR]

  bm = borderMap ts

data Border = BT | BB | BL | BR deriving (Eq, Ord, Show)

-- | The border map for a given tile contains borders of all other tiles and can
-- be used to lookup matching borders for the given tile (as it is not
-- included).
borderMap :: [Tile] -> Tile -> Map String (Border, Tile)
borderMap ts t = foldMap borders (delete t ts)

borders :: Tile -> Map String (Border, Tile)
borders t = Map.fromList
  [(top t, (BT, t)), (bottom t, (BB, t)), (left t, (BL, t)), (right t, (BR, t))]

-- | Direct and reversed lookup of tile border (flips result accordingly).
lookupBorder
  :: Map String (Border, Tile) -> Border -> Tile -> Maybe (Border, Tile)
lookupBorder bm b t = case b of
  BT -> lookup' (top t) bm
  BB -> lookup' (bottom t) bm
  BL -> lookup' (left t) bm
  BR -> lookup' (right t) bm
 where
  lookup' s m = case Map.lookup s m of
    Nothing -> mirrorResult <$> Map.lookup (reverse s) m
    Just x  -> Just x

  mirrorResult (BT, t) = (BT, flipH t)
  mirrorResult (BB, t) = (BB, flipH t)
  mirrorResult (BL, t) = (BL, flipV t)
  mirrorResult (BR, t) = (BR, flipV t)

-- | Arrange tiles to a picture by
--   - Find a corner
--   - Flip it such that it is the top-left corner
--   - Find & rotate tiles matching the right border in row until nothing on the
--     right
--   - Find & rotate tiles matching the bottom border of the first row
--   - Continue until nothing below
arrange :: [Tile] -> [[Tile]]
arrange ts = let firstRow = row' topLeftRotated in firstRow : rows firstRow
 where
  topLeft = headMay $ corners ts

  topLeftRotated = alignNothingLeft . alignNothingAbove <$> topLeft

  row' Nothing  = []
  row' (Just t) = t : row' (rightOf t)

  rows [] = []
  rows (t : _) = case below t of
    Nothing -> []
    Just r ->
      let
        first = alignNothingLeft r
        ts    = first : row first
      in ts : rows ts

  row t = case rightOf t of
    Nothing -> []
    Just r  -> r : row r

  rightOf t = alignRight <$> lookupB BR t

  alignRight = \case
    (BT, r) -> trans r
    (BB, r) -> flipH $ trans r
    (BL, r) -> r
    (BR, r) -> flipH r

  below t = alignBelow <$> lookupB BB t

  alignBelow = \case
    (BT, r) -> r
    (BB, r) -> flipV r
    (BL, r) -> trans r
    (BR, r) -> flipV $ trans r

  alignNothingLeft t = case lookupB BL t of
    Just _  -> flipH t
    Nothing -> t

  alignNothingAbove t = case lookupB BT t of
    Just _  -> flipV t
    Nothing -> t

  lookupB b t = lookupBorder (borderMap ts t) b t

display :: [[Tile]] -> String
display = concatMap displayRow
 where
  displayRow ts = merge $ map showTileRaw ts

  merge = unlines . map concat . transpose . map lines

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
