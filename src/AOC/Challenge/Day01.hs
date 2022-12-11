-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
--
module AOC.Challenge.Day01
    ( day01a
    , day01b
    ) where

import AOC.Solver ((:~>)(..))
import Data.List (sort)
import Data.List.Split (splitWhen)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)

stringToMaybeNumber :: String -> Int
stringToMaybeNumber s
    | null s = -1
    | otherwise = fromJust (readMaybe s :: Maybe Int)

parseInput :: String -> Maybe [[Int]]
parseInput = Just . splitWhen (< 0) . map stringToMaybeNumber . lines

elfWeights :: [[Int]] -> [Int]
elfWeights = map sum

largestElf :: [[Int]] -> Int
largestElf xs = maximum $ elfWeights xs

largestElves :: Int -> [[Int]] -> [Int]
largestElves n xs = take n $ reverse $ sort $ elfWeights xs

day01a :: [[Int]] :~> Int
day01a = MkSol {sParse = parseInput, sShow = show, sSolve = Just . largestElf}

day01b :: [[Int]] :~> Int
day01b =
    MkSol
        { sParse = parseInput
        , sShow = show
        , sSolve = Just . sum . largestElves 3
        }
