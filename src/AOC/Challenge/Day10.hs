-- |
-- Module      : AOC.Challenge.Day10
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
module AOC.Challenge.Day10
  ( day10a,
    day10b,
  )
where

import AOC.Solver ((:~>) (..))
import Advent.OCR (asciiMapToLetters)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

valueTranslator :: (Int, String) -> (Int, Int)
valueTranslator (x, "noop") = (x, 0)
valueTranslator (x, "addx") = (x, 0)
valueTranslator (x, v) = (x, read v :: Int)

parse :: String -> [(Int, Int)]
parse xs = zipWith (curry valueTranslator) [1 ..] (words xs)

transgression :: [(Int, Int)] -> [(Int, Int)]
transgression = init . scanl (\acc y -> (fst y, snd y + snd acc)) (0, 1)

interestingPeriods :: [(Int, Int)] -> [Int]
interestingPeriods xs = map (uncurry (*)) $ filter (isPeriod . fst) xs
  where
    isPeriod :: Int -> Bool
    isPeriod = (== 20) . (`mod` 40)

solve1 :: [(Int, Int)] -> Int
solve1 = sum . interestingPeriods . map (\(x, y) -> (x + 1, y)) . transgression

render :: [(Int, Int)] -> String
render = unlines . map (map renderPixel) . chunksOf 40
  where
    renderPixel (idx, x) = if abs (idx `mod` 40 - x) <= 1 then '#' else ' '

ocrText :: String -> String
ocrText = fromMaybe "" . asciiMapToLetters (S.singleton '#')

solve2 :: [(Int, Int)] -> String
solve2 states = ocrText $ render $ transgression states

day10a :: [(Int, Int)] :~> Int
day10a =
  MkSol
    { sParse = Just . parse,
      sShow = show,
      sSolve = Just . solve1
    }

day10b :: [(Int, Int)] :~> String
day10b =
  MkSol
    { sParse = Just . parse,
      sShow = show,
      sSolve = Just . solve2
    }
