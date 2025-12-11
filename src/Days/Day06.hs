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

readMany :: [String] -> [Int]
readMany = map read

readOperator :: String -> Int -> Int -> Int
readOperator "+" = (+)
readOperator "*" = (*)

deleteSpaces :: String -> String
deleteSpaces = filter (/=' ')

solveEquation :: ([Int], Int -> Int -> Int) -> Int
solveEquation (x:xs, op) = foldr op x xs

equationFromStrings :: ([String], String) -> ([Int], Int -> Int -> Int)
equationFromStrings (xs, op) = (readMany xs, readOperator op)

initLast :: [a] -> ([a], a)
initLast [x] = ([], x)
initLast (x:xs) = (x:xs', y) where (xs', y) = initLast xs