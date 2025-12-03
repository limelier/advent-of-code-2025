{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Days.Day03 (solve) where

solve :: String -> (Int, Int)
solve input = do
  let banks = lines input
  let part1 = sum $ map (read . bankJoltage2) banks
  let part2 = sum $ map (read . bankJoltage12) banks
  (part1, part2)

-- max 2-digit joltage in bank as string
bankJoltage2 :: String -> String
bankJoltage2 [a, b] = [a, b]
bankJoltage2 (a : t) =
  let bc = bankJoltage2 t
      [b, c] = bc
   in maximum [bc, [a, b], [a, c]]

-- max 12-digit joltage in bank as string
bankJoltage12 :: String -> String
bankJoltage12 bank
  | length bank <= 12 = bank
  | otherwise =
      let a : t = bank
          jt = bankJoltage12 t
       in max jt (a : maximum (allRemovingOne jt))

-- return all possible substrings created by removing one character
allRemovingOne :: String -> [String]
allRemovingOne [_] = [""]
allRemovingOne (h : t) = t : [h : t' | t' <- allRemovingOne t]