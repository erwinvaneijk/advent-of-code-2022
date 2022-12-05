{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
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

module AOC.Challenge.Day04
    ( day04a
    , day04b
    ) where

import           AOC.Solver                     ( (:~>)(..) )
import           Data.ExtendedReal
import qualified Data.IntegerInterval          as I
import           Data.IntegerInterval           ( (<=..<=) )
import qualified Data.IntervalRelation         as IR
import           Data.List.Split                ( wordsBy )
import           Data.Maybe                     ( fromJust )
import           Text.Read                      ( readMaybe )

stringToCoord :: [String] -> I.IntegerInterval
stringToCoord as = (Finite (coords !! 0)) <=..<= (Finite (coords !! 1))
    where
        coords = map sToItem as
        sToItem :: String -> Integer
        sToItem = fromJust . readMaybe

itemSplit :: String -> (I.IntegerInterval, I.IntegerInterval)
itemSplit s = (coords !! 0, coords !! 1)
    where
        coords = map stringToCoord $ map (wordsBy (== '-')) $ wordsBy (== ',') s

parse :: String -> [(I.IntegerInterval, I.IntegerInterval)]
parse = map itemSplit . lines

containedIn :: (I.IntegerInterval, I.IntegerInterval) -> Bool
containedIn (x, y) = I.isSubsetOf x y || I.isSubsetOf y x

-- This is a bit convoluted, but I wanted to use this relate operation
overlaps :: (I.IntegerInterval, I.IntegerInterval) -> Bool
overlaps (x, y) =
    I.relate x y
    `elem` [ IR.Overlaps
           , IR.Starts
           , IR.During
           , IR.Equal
           , IR.Contains
           , IR.Finishes
           ]
    ||
    I.relate y x
    `elem` [ IR.Overlaps
           , IR.Starts
           , IR.During
           , IR.Equal
           , IR.Contains
           , IR.Finishes
           ]

day04a :: _ :~> _
day04a = MkSol { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . length . filter containedIn
  }

day04b :: _ :~> Int
day04b = MkSol { sParse = Just . parse
  , sShow  = show
  , sSolve = Just . length . filter overlaps
  }
