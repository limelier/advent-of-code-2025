{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Days.Day01 (solve) where


solve :: String -> (Int, Int)
solve input = do
  let instructions = map numericInstruction $ lines input
  doSpins 50 instructions 0 0

-- translate R## to ##, and L## to -##
numericInstruction :: String -> Int
numericInstruction ('R' : t) = read t
numericInstruction ('L' : t) = -read t

-- starting from pos (1), apply instructions (2)
-- accumulating zeroes between instructions in (3), returning in (5)
-- accumulating all encountered zeroes in (4), returning in (6)
--- zeroesBetween are counted when the instruction is done, zeroes once the next instruction starts or none are left
doSpins :: Int -> [Int] -> Int -> Int -> (Int, Int)
-- handle wrap-around first
doSpins (-1) instrs zeroesBetween zeroes = doSpins 99 instrs zeroesBetween zeroes
doSpins 100 instrs zeroesBetween zeroes = doSpins 0 instrs zeroesBetween zeroes
-- base case: instructions done
doSpins 0 [] zeroesBetween zeroes = (zeroesBetween, zeroes + 1)
doSpins _ [] zeroesBetween zeroes = (zeroesBetween, zeroes)
-- current instruction is done
-- - ... on a zero
doSpins 0 (0 : instrTail) zeroesBetween zeroes = doSpins 0 instrTail (zeroesBetween + 1) zeroes
-- - ... not on a zero
doSpins pos (0 : instrTail) zeroesBetween zeroes = doSpins pos instrTail zeroesBetween zeroes
-- current instruction is not done
doSpins pos (instr : instrTail) zeroesBetween zeroes =
  let (pos', instr') = if instr > 0 then (pos + 1 `mod` 100, instr - 1) else (pos - 1, instr + 1)
      zeroes' = if pos == 0 then zeroes + 1 else zeroes
   in doSpins pos' (instr' : instrTail) zeroesBetween zeroes'
