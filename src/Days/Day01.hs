{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day01 (solve) where

solve :: String -> (Int, Int)
solve input = do
  let instructions = lines input
  (doSpins instructions 50 0, 0)

--- part 1
-- doSpins instructions position zeroes -> zeroes
doSpins :: [String] -> Int -> Int -> Int
doSpins [] _ zeroes = zeroes
doSpins (ins : rest) pos zeroes =
  if newPos == 0
    then doSpins rest newPos (zeroes + 1)
    else doSpins rest newPos zeroes
  where
    newPos = doSpin ins pos

-- doSpin instruction position -> newPos
doSpin :: String -> Int -> Int
doSpin (lr : num) pos = (pos + lrMult lr * read num) `mod` 100

--- common
lrMult :: Char -> Int
lrMult 'R' = 1
lrMult 'L' = -1
