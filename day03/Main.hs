{-# LANGUAGE ScopedTypeVariables #-}
module Main where

step :: Int -> Int -> [[Char]] -> [[Char]]
step right down m = map (drop right) $ drop down m

countTrees :: Int -> Int -> [[Char]] -> Int
countTrees _ _ [] = 0
countTrees _ _ [[]] = 0
countTrees right down m@((x:_):_)
  | x == '#' = 1 + countTrees right down (step right down m)
  | otherwise = countTrees right down (step right down m)

part1 :: String -> String
part1 input =
  let m = map cycle $ lines input
  in  show $ countTrees 3 1 m

part2 :: String -> String
part2 input = do
  let m = map cycle $ lines input
  show $ product
    [ countTrees 1 1 m
    , countTrees 3 1 m
    , countTrees 5 1 m
    , countTrees 7 1 m
    , countTrees 1 2 m
    ]

main :: IO ()
main = do
  -- let input = "..##.......\n\
  --             \#...#...#..\n\
  --             \.#....#..#.\n\
  --             \..#.#...#.#\n\
  --             \.#...##..#.\n\
  --             \..#.##.....\n\
  --             \.#.#.#....#\n\
  --             \.#........#\n\
  --             \#.##...#...\n\
  --             \#...##....#\n\
  --             \.#..#...#.#\n"
  input <- readFile "input"
  putStrLn "part one"
  putStrLn $ part1 input
  putStrLn "part two"
  putStrLn $ part2 input
