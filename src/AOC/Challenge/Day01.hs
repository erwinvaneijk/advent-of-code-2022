-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import           AOC.Solver ((:~>)(..))
import           AOC.Common (slidingWindows, slidingPairs, countTrue)
import           Text.Read (readMaybe)

parseInput :: String -> Maybe [Int]
parseInput = traverse readMaybe . lines

countIncreases :: [Int] -> Int
countIncreases = countTrue (uncurry (<)) . slidingPairs

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = Just . countIncreases
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = Just . countIncreases . map sum . slidingWindows 3
    }

