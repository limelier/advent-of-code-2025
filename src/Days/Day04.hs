module Days.Day04 (solve) where

import Data.Matrix as Matrix
import Data.Maybe

solve :: String -> (Int, Int)
solve input = do
  let m = Matrix.fromLists $ lines input
  (numAccessiblePaper m, numEventuallyAccessiblePaper 0 m)

-- get number of paper stacks that are accessible
numAccessiblePaper :: Matrix Char -> Int
numAccessiblePaper m = length $ accessiblePaper m

-- recursively remove all accessible paper until unable; accumulate and return number of paper removed
numEventuallyAccessiblePaper :: Int -> Matrix Char -> Int
numEventuallyAccessiblePaper removed m =
    let xs = accessiblePaper m
        m' = removeMany m xs
    in if null xs
        then removed
        else numEventuallyAccessiblePaper (removed + length xs) m'

-- remove paper from a matrix given its positions
removeMany :: Matrix Char -> [(Int, Int)] -> Matrix Char
removeMany = foldl (flip (unsafeSet '.'))

accessiblePaper :: Matrix Char -> [(Int, Int)]
accessiblePaper m =
  [ (i, j)
    | i <- [1 .. (nrows m)],
      j <- [1 .. (ncols m)],
      accessible i j m,
      m ! (i, j) == '@'
  ]


-- check if position (i, j) in m is accessible
accessible :: Int -> Int -> Matrix Char -> Bool
accessible i j m = length (filter isPaper $ neighbors i j m) < 4

isPaper :: Char -> Bool
isPaper c = c == '@'

-- get the 8 neighbors of pos (i, j) in m
neighbors :: Int -> Int -> Matrix Char -> [Char]
neighbors i j m =
  catMaybes
    [ safeGet i' j' m
      | i' <- [(i - 1) .. (i + 1)],
        j' <- [(j - 1) .. (j + 1)],
        (i', j') /= (i, j)
    ]