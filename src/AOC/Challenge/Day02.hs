-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day ${day_short}.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.
module AOC.Challenge.Day02
    ( day02a
    , day02b
    ) where

import AOC.Solver ((:~>)(..))
import Data.List.Split (splitOn)

-- A, X = Rock, 1
-- B, Y = Paper, 2
-- C, Z = Scissors, 3
-- Loss = 0, Draw = 3, Win = 6
rockPaperScissors :: [String] -> Int
rockPaperScissors ["A", "X"] = 1 + 3
rockPaperScissors ["A", "Y"] = 2 + 6
rockPaperScissors ["A", "Z"] = 3 + 0
rockPaperScissors ["B", "X"] = 1 + 0
rockPaperScissors ["B", "Y"] = 2 + 3
rockPaperScissors ["B", "Z"] = 3 + 6
rockPaperScissors ["C", "X"] = 1 + 6
rockPaperScissors ["C", "Y"] = 2 + 0
rockPaperScissors ["C", "Z"] = 3 + 3
rockPaperScissors _ = -1

-- A = Rock, 1
-- B = Paper, 2
-- C = Scissors, 3
-- Loss = X, Draw = Y, Win = Z
rockPaperScissorsRules2 :: [String] -> Int
rockPaperScissorsRules2 ["A", "X"] = 3
rockPaperScissorsRules2 ["A", "Y"] = 1 + 3
rockPaperScissorsRules2 ["A", "Z"] = 2 + 6
rockPaperScissorsRules2 ["B", "X"] = 1 + 0
rockPaperScissorsRules2 ["B", "Y"] = 2 + 3
rockPaperScissorsRules2 ["B", "Z"] = 3 + 6
rockPaperScissorsRules2 ["C", "X"] = 2 + 0
rockPaperScissorsRules2 ["C", "Y"] = 3 + 3
rockPaperScissorsRules2 ["C", "Z"] = 1 + 6
rockPaperScissorsRules2 _ = -1

doGame1 :: [[String]] -> Int
doGame1 xs = sum $ map rockPaperScissors xs

doGame2 :: [[String]] -> Int
doGame2 xs = sum $ map rockPaperScissorsRules2 xs

parseInput :: String -> Maybe [[String]]
--parseInput = traverse readMaybe . lines
parseInput s = Just $ map (splitOn " ") $ lines s

day02a :: [[String]] :~> Int
day02a = MkSol {sParse = parseInput, sShow = show, sSolve = Just . doGame1}

day02b :: [[String]] :~> Int
day02b = MkSol {sParse = parseInput, sShow = show, sSolve = Just . doGame2}
