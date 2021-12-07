{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day07 where

import Control.Arrow ((&&&))
import Data.List (sort, group, foldl')

import Harness
import ParseHelper


main :: IO ()
main = do
    getInputAndSolve
        (parseInputRaw $ parseIntArray Nothing Nothing <* newline)
        (alignCrabs id)
        (alignCrabs $ \moves -> sum [1 .. moves])

alignCrabs :: (Int -> Int) -> [Int] -> Int
alignCrabs burnCost initialPos =
    let maxPos = maximum initialPos
        posCounts = map (head &&& length) . group $ sort initialPos
    in  minimum $ map (calculateCost posCounts) [0 .. maxPos]
    where
        calculateCost :: [(Int, Int)] -> Int -> Int
        calculateCost pos target =
            foldl'
                (\c (position, count) ->
                    c + burnCost (abs (target - position)) * count
                ) 0 pos
