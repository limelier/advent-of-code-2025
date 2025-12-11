{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Days.Day05 (solve) where

import Data.List.Split
import Data.List (sortBy)

solve :: String -> (Int, Int)
solve input = do
  let [rangesLines, idLines] = splitOn [""] $ lines input
  let ranges = mergeRanges $ map stringToRange rangesLines
  let ids = map read idLines
  let fresh = filter (`inRanges` ranges) ids
  (length fresh, sum $ map rangeLen ranges)

data Range = Range Int Int

-- convert "##-##" string to range
stringToRange :: String -> Range
stringToRange s = Range lo hi where [lo, hi] = map read $ splitOn "-" s

-- check if x in range [lo, hi]
inRange :: Int -> Range -> Bool
inRange x (Range lo hi) = x >= lo && x <= hi

-- check if x in any of the ranges
inRanges :: Int -> [Range] -> Bool
inRanges x = any (x `inRange`)

-- get the length of (num of numbers in) a range
rangeLen :: Range -> Int
rangeLen (Range lo hi) = hi - lo + 1

-- merge two [lo1, hi1] and [lo2, hi2] ranges if they intersect
mergeTwoRanges :: Range -> Range -> Maybe Range
mergeTwoRanges (Range lo1 hi1) (Range lo2 hi2)
  | lo1 < lo2 && hi1 < lo2 = Nothing
  | lo2 < lo1 && hi2 < lo1 = Nothing
  | otherwise = Just $ Range (min lo1 lo2) (max hi1 hi2)

-- sort and merge all intersecting ranges
mergeRanges :: [Range] -> [Range]
mergeRanges ranges = mergeSortedRanges $ sortBy compareRanges ranges
    where compareRanges (Range lo1 _) (Range lo2 _) = compare lo1 lo2

-- given sorted ranges, merge all intersecting ones
mergeSortedRanges :: [Range] -> [Range]
mergeSortedRanges [] = []
mergeSortedRanges [r] = [r]
mergeSortedRanges (r1:r2:rest) = case mergeTwoRanges r1 r2 of
    Nothing -> r1:mergeSortedRanges (r2:rest)
    Just r -> mergeSortedRanges (r:rest)