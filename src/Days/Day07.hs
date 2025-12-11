{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day07 where

import Data.Set (Set, difference, intersection, union)
import qualified Data.Set as Set (fromList, map)

solve :: String -> (Int, Int)
solve input = do
  let ics = map (map fst . indexedSymbols) $ lines input
  let start : rest = ics
  let (finalBeams, totalSplits) = applySplittersRec (Set.fromList start) rest
  (totalSplits, 0)

indexedSymbols :: String -> [(Int, Char)]
indexedSymbols s = filter (\(_, c) -> c /= '.') $ indexed s

indexed :: [a] -> [(Int, a)]
indexed = zip [0 ..]

applySplitters :: Set Int -> [Int] -> (Set Int, Int)
applySplitters beams splitters = do
  let hits = beams `intersection` Set.fromList splitters
  let left = Set.map pred hits
  let right = Set.map succ hits
  let beams' = beams `difference` hits `union` left `union` right
  (beams', length hits)

applySplittersRec :: Set Int -> [[Int]] -> (Set Int, Int)
applySplittersRec beams [] = (beams, 0)
applySplittersRec beams (ss : sss) = (finalBeams, splits + restSplits)
  where
    (beams', splits) = applySplitters beams ss
    (finalBeams, restSplits) = applySplittersRec beams' sss