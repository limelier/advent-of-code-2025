module Main where

import Days.Day06 (solve)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (part1, part2) = solve input
  print $ "Part 1 = " ++ show part1
  print $ "Part 2 = " ++ show part2
