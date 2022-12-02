{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

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

module AOC.Challenge.Day02 (
    -- day02a
  -- , day02b
  ) where

import           AOC.Prelude

import qualified Data.Graph.Inductive           as G
import qualified Data.IntMap                    as IM
import qualified Data.IntSet                    as IS
import qualified Data.List.NonEmpty             as NE
import qualified Data.List.PointedList          as PL
import qualified Data.List.PointedList.Circular as PLC
import qualified Data.Map                       as M
import qualified Data.OrdPSQ                    as PSQ
import qualified Data.Sequence                  as Seq
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Vector                    as V
import qualified Linear                         as L
import qualified Text.Megaparsec                as P
import qualified Text.Megaparsec.Char           as P
import qualified Text.Megaparsec.Char.Lexer     as PP

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

doGame :: [[String]] -> Int
doGame xs = sum $ map rockPaperScissors xs

parseInput :: String -> Maybe [[String]]
--parseInput = traverse readMaybe . lines
parseInput s = Just $ map (splitOn " ") $ lines s

day02a :: [[String]] :~> Int
day02a = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = Just . doGame
    }

day02b :: [[String]] :~> Int
day02b = MkSol
    { sParse = parseInput
    , sShow  = show
    , sSolve = Just . doGame
    }
