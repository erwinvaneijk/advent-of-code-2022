{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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
                           , height
                           , parse
                           , Tree(..)
                           , isVisibleFromTop
                           , isVisibleFromBottom
                           , isVisibleFromLeft
                           , isVisibleFromRight
                           , isVisible
                           , visibleTrees
                           , numVisible
                           , scenicScore
                           , treesLower
                           , treesLowerToLeft
                           , treesLowerToRight
                           , treesLowerToTop
                           , treesLowerToBottom
                           , colToTop
                           , colToBottom
                           , rowToLeft
                           , rowToRight
  ) where

import           AOC.Prelude

import Data.Char (digitToInt)
import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.Matrix                    as MA
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

data Tree = Tree Int
            deriving stock (Show)

height :: Tree -> Int
height (Tree h) = h

parse :: String -> MA.Matrix Tree
parse xs = MA.fromList numRows numCols $ map (\x -> Tree x) $ map digitToInt $ concat $ lines xs
    where
        theLines = lines xs
        numRows = length theLines
        numCols = length $ head theLines

rowToLeft :: Int -> Int -> MA.Matrix Tree -> V.Vector Tree
rowToLeft row col m = (V.take (col - 1) $ MA.getRow row m)
rowToRight :: Int -> Int -> MA.Matrix Tree -> V.Vector Tree
rowToRight row col m = (V.drop col $ MA.getRow row m)
colToTop :: Int -> Int -> MA.Matrix Tree -> V.Vector Tree
colToTop row col m = (V.take (row - 1) $ MA.getCol col m)
colToBottom :: Int -> Int -> MA.Matrix Tree -> V.Vector Tree
colToBottom row col m = (V.drop row $ MA.getCol col m)

isVisibleFromTop :: Int -> Int -> MA.Matrix Tree -> Bool
isVisibleFromTop row col m = V.all (\x -> height x < h) (colToTop row col m)
    where
        h = height $ MA.getElem row col m

isVisibleFromBottom :: Int -> Int -> MA.Matrix Tree -> Bool
isVisibleFromBottom row col m = V.all (\x -> height x < h) (colToBottom row col m)
    where
        h = height $ MA.getElem row col m

isVisibleFromLeft :: Int -> Int -> MA.Matrix Tree -> Bool
isVisibleFromLeft row col m = V.all (\x -> height x < h) (rowToLeft row col m)
    where
        h = height $ MA.getElem row col m

isVisibleFromRight :: Int -> Int -> MA.Matrix Tree -> Bool
isVisibleFromRight row col m = V.all (\x -> height x < h) (rowToRight row col m)
    where
        h = height $ MA.getElem row col m

isVisible :: Int -> Int -> MA.Matrix Tree -> Bool
isVisible 1   _ _   = True
isVisible _   1 _   = True
isVisible _   col m | (MA.ncols m) == col = True
isVisible row _   m | (MA.nrows m) == row = True
isVisible row col m = isVisibleFromLeft row col m || isVisibleFromRight row col m || isVisibleFromTop row col m || isVisibleFromBottom row col m

visibleTrees :: MA.Matrix Tree -> [Bool]
visibleTrees m = [isVisible r c m | r <- [1..(MA.nrows m)], c <- [1..(MA.ncols m)]]

numVisible :: MA.Matrix Tree -> Int
numVisible m = length $ filter (==True) $ visibleTrees m

treesLower :: Int -> V.Vector Tree -> Int
treesLower n v = case V.findIndex (\x -> n <= height x) v of
                     Nothing -> V.length v
                     Just val -> val + 1

treesLowerToLeft :: Int -> Int -> MA.Matrix Tree -> Int
treesLowerToLeft row col m = treesLower h $ V.reverse (rowToLeft row col m)
    where
        h = height $ MA.getElem row col m

treesLowerToRight :: Int -> Int -> MA.Matrix Tree -> Int
treesLowerToRight row col m = treesLower h (rowToRight row col m)
    where
        h = height $ MA.getElem row col m

treesLowerToTop :: Int -> Int -> MA.Matrix Tree -> Int
treesLowerToTop row col m = treesLower h $ V.reverse (colToTop row col m)
    where
        h = height $ MA.getElem row col m

treesLowerToBottom :: Int -> Int -> MA.Matrix Tree -> Int
treesLowerToBottom row col m = treesLower h (colToBottom row col m)
    where
        h = height $ MA.getElem row col m

scenicScore :: Int -> Int -> MA.Matrix Tree -> Int
scenicScore row col m = treesLowerToLeft row col m *
                         treesLowerToRight row col m *
                         treesLowerToTop row col m *
                         treesLowerToBottom row col m

maxScenicScore :: MA.Matrix Tree -> Int
maxScenicScore m = maximum $ [ scenicScore r c m | r <- [1..(MA.nrows m)], c <- [1..(MA.ncols m)]]

day08a :: MA.Matrix Tree :~> Int
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
