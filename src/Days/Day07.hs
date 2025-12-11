{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day07 where

import GHC.Arr

solve :: String -> (Int, Int)
solve input = do
  let (startLine : splitterLines) = lines input
  let beams = listArray (0, length startLine - 1) [if c == 'S' then (1 :: Int) else 0 | c <- startLine]
  let (finalBeams, splits) = splitRec beams splitterLines
  (splits, sum finalBeams)

-- recursively split the beam front with each line of splitters from the input
-- return the final beam front and total number of splits
splitRec :: Array Int Int -> [String] -> (Array Int Int, Int)
splitRec beams [] = (beams, 0)
splitRec beams (line:rest) = do
  let splitters = splittersOf line
  let (beams', splits) = splitMany beams splitters
  let (finalBeams, remainingSplits) = splitRec beams' rest
  (finalBeams, splits + remainingSplits)

-- split the beam front with the given list of splitters
-- return the new beam front and the number of splits
splitMany :: Array Int Int -> [Int] -> (Array Int Int, Int)
splitMany beams splitters = do
  let splitters' = filter (\s -> beams!s /= 0) splitters
  let beams' = foldl split beams splitters
  (beams', length splitters')

-- split the beam at pos into two copies, adding them to the previous and next beams
split :: Array Int Int -> Int -> Array Int Int
split beams pos =
  beams
    // [ (pos, 0),
         (left, beams ! left + beams ! pos),
         (right, beams ! right + beams ! pos)
       ]
  where
    left = pos - 1
    right = pos + 1

-- parse an input line to a list of splitter indices
splittersOf :: String -> [Int]
splittersOf s = map fst $ filter (\(_, c) -> c == '^') $ indexed s

-- pair each element of the list with its index
indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]