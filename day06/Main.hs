module Main where

part1 :: String -> String
part1 = id

part2 :: String -> String
part2 = id

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn "Part one"
  putStrLn $ "> " <> part1 input
  putStrLn "Part two"
  putStrLn $ "> " <> part2 input
