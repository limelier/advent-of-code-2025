{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Days.Day02 (solve) where

import Data.List.Split

solve :: String -> (Int, Int)
solve input = do
  let ranges = map rangeOf (splitOn "," input)
  (part1 ranges, part2 ranges)

-- part 1: sum the invalid ids in the ranges
part1 :: [Range] -> Int
part1 ranges =
  sum $ concatMap (filter invalid1 . idsInRange) ranges

-- part 2: do it again but with invalid2
part2 :: [Range] -> Int
part2 ranges =
  sum $ concatMap (filter invalid2 . idsInRange) ranges

-- Ranges are pairs of IDs, start and end
data Range = Range ID ID

-- range from "start-end" string
rangeOf :: String -> Range
rangeOf rangeString = Range (read s) (read e) where [s, e] = splitOn "-" rangeString

-- generate all ids in the range
idsInRange :: Range -> [ID]
idsInRange (Range start end) = [start..end]

-- IDs are numbers
type ID = Int

-- check if ID is invalid (part 1)
invalid1 :: ID -> Bool
invalid1 identifier =
  l == r
  where
    (l, r) = bisect $ show identifier

-- check if ID is invalid (part 2)
invalid2 :: ID -> Bool
invalid2 identifier =
  any allEqual [chunksOf len str | len <- [1..length str `div` 2]]
  where str = show identifier

-- check if all strings in list are equal
allEqual :: [String] -> Bool
allEqual [] = True
allEqual [_] = True
allEqual [a,b] = a == b
allEqual (a:b:rest) = a == b && allEqual (b:rest)

-- cut string into halves
bisect :: String -> (String, String)
bisect string =
  splitAt ((length string + 1) `div` 2) string
