{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Days.Day06 (solve) where

import Data.List
import Data.List.Split

solve :: String -> (Int, Int)
solve input = do
    (part1 input, part2 input)

part1 :: String -> Int
part1 input = do
    let wordsInLines = map words $ lines input
    let equations = map (equationFromStrings . initLast) $ transpose wordsInLines
    let results = map solveEquation equations
    sum results

part2 :: String -> Int
part2 input = do
    let (numberLines, operatorLine) = initLast $ lines input
    let numberStrings = splitWhen null $ map deleteSpaces $ transpose numberLines
    let opStrings = words operatorLine
    let equations = zipWith (curry equationFromStrings) numberStrings opStrings
    let results = map solveEquation equations
    sum results

-- parse a bunch of numbers at once
readMany :: [String] -> [Int]
readMany = map read

-- parse operators. haskell is kinda cool here im ngl
readOperator :: String -> Int -> Int -> Int
readOperator "+" = (+)
readOperator "*" = (*)

-- delete all spaces in a string
deleteSpaces :: String -> String
deleteSpaces = filter (/=' ')

-- solve an equation by folding the operator over all the operands
solveEquation :: ([Int], Int -> Int -> Int) -> Int
solveEquation (x:xs, op) = foldr op x xs

-- parse strings to an equation
equationFromStrings :: ([String], String) -> ([Int], Int -> Int -> Int)
equationFromStrings (xs, op) = (readMany xs, readOperator op)

-- get both init and last at the same time
initLast :: [a] -> ([a], a)
initLast [x] = ([], x)
initLast (x:xs) = (x:xs', y) where (xs', y) = initLast xs