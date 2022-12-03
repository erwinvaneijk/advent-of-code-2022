{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
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

module AOC.Challenge.Day03 (
                             day03a
                           , day03b
                           , parseInput
                           , doubleItems
                           , split
                           , score
                           , rucksacks
                           , solveDay3
  ) where

import           AOC.Solver ((:~>)(..))
import Data.Char (ord)
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

parseInput :: String -> Maybe [String]
parseInput s = Just $ lines s

split :: [a] -> ([a], [a])
split l = splitAt (((length l) + 1) `div` 2) l

priority :: Char -> Int
priority x
  | x>='a' && x <= 'z' = 1 + (ord x) - (ord 'a')
  | x>='A' && x <= 'Z' = 27 + (ord x) - (ord 'A')
  | otherwise = 0

score :: [Char] -> Int
score = sum . map priority

-- split the input in two equal sizes
rucksacks :: [a] -> ([a], [a])
rucksacks = split

doubleItems :: Ord a => ([a], [a]) -> [a]
doubleItems rs = S.toList $ sl `S.intersection` sr
    where
        sl = S.fromList $ fst rs
        sr = S.fromList $ snd rs

solveDay3 :: [String] -> Int
solveDay3 = sum . map (score . doubleItems . rucksacks)

day03a :: [String] :~> Int
day03a = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = Just . solveDay3
    }

day03b :: [String] :~> Int
day03b = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = Just . solveDay3
    }
