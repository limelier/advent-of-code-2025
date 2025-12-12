{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day08 where

import Data.List (sortOn)
import Data.Map (Map, assocs, insert, member, (!), keysSet, elems)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Ord
import Data.Tuple (swap)

type Pos = (Int, Int, Int)

type Pair = (Pos, Pos)

solve :: String -> (Int, Int)
solve input = do
  let positions = map parseLine $ lines input
  let pairs = sortOn (uncurry distanceSq) $ pair positions
  let initialMap = Map.fromList $ zip positions (map show positions)

  let (circuits, _) = connectNearest1000 pairs initialMap []
  let circuits' = flipCircuits circuits
  let topThree = take 3 $ sortOn (Data.Ord.Down . length) (Map.elems circuits')

  let (_, links) = connectUntilUnified pairs initialMap []
  let ((x1, _, _), (x2, _, _)) = head links
  (product $ map length topThree, x1 * x2)

parseLine :: String -> Pos
parseLine line = read $ "(" ++ line ++ ")"

distanceSq :: Pos -> Pos -> Int
distanceSq (a, b, c) (d, e, f) = ((d - a) ^ 2) + ((e - b) ^ 2) + ((f - c) ^ 2)

pair :: [a] -> [(a, a)]
pair [] = []
pair (x : xs) = map (x,) xs ++ pair xs

connectNearest1000 :: [Pair] -> Map Pos String -> [Pair] -> (Map Pos String, [Pair])
connectNearest1000 [] circuits links = (circuits, links)
connectNearest1000 (p@(a, b) : ps) circuits links
  | length links >= 1000 = (circuits, links)
  | otherwise = do
      let f x = if x == (circuits ! b) then circuits ! a else x
      let circuits' = Map.map f circuits
      connectNearest1000 ps circuits' (p : links)

connectUntilUnified :: [Pair] -> Map Pos String -> [Pair] -> (Map Pos String, [Pair])
connectUntilUnified [] circuits links = (circuits, links)
connectUntilUnified (p@(a, b) : ps) circuits links
  | Set.size (Set.fromList $ elems circuits) == 1 = (circuits, links)
  | otherwise = do
      let f x = if x == (circuits ! b) then circuits ! a else x
      let circuits' = Map.map f circuits
      connectUntilUnified ps circuits' (p : links)

flipCircuits :: Map Pos String -> Map String [Pos]
flipCircuits boxToCircuit = combine Map.empty (map swap (assocs boxToCircuit))

combine :: Map String [Pos] -> [(String, Pos)] -> Map String [Pos]
combine m [] = m
combine m ((k, v) : xs)
  | member k m = combine (insert k (v : m ! k) m) xs
  | otherwise = combine (insert k [v] m) xs