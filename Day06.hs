{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day06 where

import Control.Arrow ((&&&))
import Data.Function (on)
import Data.List (sort, sortOn, group, partition, unionBy)

import Harness
import ParseHelper

import qualified GHC.Arr as A


main :: IO ()
main = do
    getInputAndSolve
        (parseInputRaw $ parseIntArray Nothing Nothing <* newline)
        (breed 80)
        (breedFast 256)


-- Naive list of timers
breed :: Int -> [Int] -> Int
breed days timers
    | days == 0 = length timers
    | otherwise = breed (days - 1) tickTimers
    where
        tickTimers :: [Int]
        tickTimers =
            let (length -> zeroCount, map pred -> nonZeroTimers) = partition (== 0) timers
            in
                replicate zeroCount 6 <> replicate zeroCount 8 <> nonZeroTimers

-- Array of counts indexed by time remaining
breedFast :: Int -> [Int] -> Int
breedFast days timers = go days initialState
    where
        initialState :: A.Array Int Int
        initialState =
            let existingFish = map (head &&& length) . group $ sort timers
                zeros = [(x, 0) | x <- [0 .. 8]]
            in A.array (0, 8) $ sortOn fst $ unionBy ((==) `on` fst) existingFish zeros
        stateToCount :: A.Array Int Int -> Int
        stateToCount =
            A.foldrElems' (+) 0
        go :: Int -> A.Array Int Int -> Int
        go d arr
            | d == 0 =
                stateToCount arr
            | otherwise =
                let zeroCount = arr A.! 0
                    ticked = rotate arr
                    breeded = A.accum (+) ticked [(6, zeroCount), (8, zeroCount)]
                in go (d - 1) breeded
        -- push everything to the left 1 index, dropping the first element
        rotate :: A.Array Int Int -> A.Array Int Int
        rotate arr =
            let (_ : vs) = A.elems arr
            in A.listArray (0, 8) $ vs <> [0]
