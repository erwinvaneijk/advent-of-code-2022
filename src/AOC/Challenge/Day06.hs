-- |
-- Module      : AOC.Challenge.Day06
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
module AOC.Challenge.Day06
    ( day06a
    , day06b
    , findMarker
    ) where

import AOC.Common (slidingWindows)
import AOC.Solver ((:~>)(..))
import qualified Data.Foldable as DF
import qualified Data.Set as S
import qualified Data.Text as T

findMarker :: Int -> String -> Int
findMarker n xs = n + snd (head $ uniqueWindows xs)
  where
    uniqueWindows fs =
        filter (\x -> n == S.size (S.fromList $ DF.toList $ fst x)) $
        zip (slidingWindows n fs) [0 ..]

day06a :: String :~> Int
day06a =
    MkSol
        { sParse = Just . head . lines
        , sShow = show
        , sSolve = Just . findMarker 4
        }

day06b :: _ :~> _
day06b =
    MkSol {sParse = sParse day06a, sShow = show, sSolve = Just . findMarker 14}
