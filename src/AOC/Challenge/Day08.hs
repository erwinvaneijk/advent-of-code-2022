-- |
-- Module      : AOC.Challenge.Day08
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

module AOC.Challenge.Day08 (
                             day08a
                           , day08b
  ) where

import           AOC.Solver                     ( (:~>)(..) )
import Data.Char (digitToInt)
import qualified Data.Matrix                    as MA
import qualified Data.Vector                    as V

parse :: String -> MA.Matrix Int
parse xs = MA.fromList numRows numCols $ map digitToInt $ concat $ lines xs
    where
        theLines = lines xs
        numRows = length theLines
        numCols = length $ head theLines

rowToLeft :: Int -> Int -> MA.Matrix Int -> V.Vector Int
rowToLeft row col m = (V.take (col - 1) $ MA.getRow row m)
rowToRight :: Int -> Int -> MA.Matrix Int -> V.Vector Int
rowToRight row col m = (V.drop col $ MA.getRow row m)

colToTop :: Int -> Int -> MA.Matrix Int -> V.Vector Int
colToTop row col m = (V.take (row - 1) $ MA.getCol col m)
colToBottom :: Int -> Int -> MA.Matrix Int -> V.Vector Int
colToBottom row col m = (V.drop row $ MA.getCol col m)

isVisibleFromTop :: Int -> Int -> MA.Matrix Int -> Bool
isVisibleFromTop row col m = V.all (< h) (colToTop row col m)
    where
        h = MA.getElem row col m

isVisibleFromBottom :: Int -> Int -> MA.Matrix Int -> Bool
isVisibleFromBottom row col m = V.all (< h) (colToBottom row col m)
    where
        h = MA.getElem row col m

isVisibleFromLeft :: Int -> Int -> MA.Matrix Int -> Bool
isVisibleFromLeft row col m = V.all (\x -> x < h) (rowToLeft row col m)
    where
        h = MA.getElem row col m

isVisibleFromRight :: Int -> Int -> MA.Matrix Int -> Bool
isVisibleFromRight row col m = V.all (\x -> x < h) (rowToRight row col m)
    where
        h = MA.getElem row col m

isVisible :: Int -> Int -> MA.Matrix Int -> Bool
isVisible 1   _ _   = True
isVisible _   1 _   = True
isVisible _   col m | (MA.ncols m) == col = True
isVisible row _   m | (MA.nrows m) == row = True
isVisible row col m = isVisibleFromLeft row col m || isVisibleFromRight row col m || isVisibleFromTop row col m || isVisibleFromBottom row col m

visibleInts :: MA.Matrix Int -> [Bool]
visibleInts m = [isVisible r c m | r <- [1..(MA.nrows m)], c <- [1..(MA.ncols m)]]

numVisible :: MA.Matrix Int -> Int
numVisible m = length $ filter (==True) $ visibleInts m

treesLower :: Int -> V.Vector Int -> Int
treesLower n v = case V.findIndex (>= n) v of
                     Nothing -> V.length v
                     Just val -> val + 1

height :: Int -> Int -> MA.Matrix Int -> Int
height = MA.getElem

treesLowerToLeft :: Int -> Int -> MA.Matrix Int -> Int
treesLowerToLeft row col m = treesLower (height row col m) $ V.reverse (rowToLeft row col m)

treesLowerToRight :: Int -> Int -> MA.Matrix Int -> Int
treesLowerToRight row col m = treesLower (height row col m) (rowToRight row col m)

treesLowerToTop :: Int -> Int -> MA.Matrix Int -> Int
treesLowerToTop row col m = treesLower (height row col m) $ V.reverse (colToTop row col m)

treesLowerToBottom :: Int -> Int -> MA.Matrix Int -> Int
treesLowerToBottom row col m = treesLower (height row col m) (colToBottom row col m)

scenicScore :: Int -> Int -> MA.Matrix Int -> Int
scenicScore row col m = treesLowerToLeft row col m *
                         treesLowerToRight row col m *
                         treesLowerToTop row col m *
                         treesLowerToBottom row col m

maxScenicScore :: MA.Matrix Int -> Int
maxScenicScore m = maximum $ [ scenicScore r c m | r <- [1..(MA.nrows m)], c <- [1..(MA.ncols m)]]

day08a :: MA.Matrix Int :~> Int
day08a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . numVisible
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . maxScenicScore
    }
