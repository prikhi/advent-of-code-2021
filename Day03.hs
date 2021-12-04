{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Bifunctor (bimap)
import Data.Bits (shiftL, (.|.), setBit, testBit, complementBit, finiteBitSize, countLeadingZeros)
import Data.Functor (($>))
import Data.List (transpose, partition, foldl')
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

main :: IO ()
main = do
    rawInput <- getRawInput
    solve "Bool Matrix" (parseInput parseBitArray) calculatePowerConsumption calculateLifeSupportRating rawInput

    putStrLn ""

    let intArrayParser = \str ->
            let !res = parseInput parseBitInt str
                !bitSize = finiteBitSize (1 :: Int) - minimum (map countLeadingZeros res)
            in  (bitSize, res)
    solve "Int Array" intArrayParser (uncurry cPCInt) (uncurry cLSRInt) rawInput


parseBitArray :: ReadP [Bool]
parseBitArray =
    many1 $ choice
        [ char '0' $> False
        , char '1' $> True
        ]

parseBitInt :: ReadP Int
parseBitInt =
    foldl' (\i b -> shiftL i 1 .|. b) 0
        <$> many1 (choice [char '1' $> 1, char '0' $> 0])

calculatePowerConsumption :: [[Bool]] -> Int
calculatePowerConsumption (transpose -> matrix) =
    let gammaBits = map calculateGammaBit matrix
        epsilonBits = map not gammaBits
    in  bitsToInt gammaBits * bitsToInt epsilonBits
    where
        calculateGammaBit :: [Bool] -> Bool
        calculateGammaBit bits =
            let (onCount, offCount) = bimap length length $ partition id bits
            in onCount > offCount

cPCInt :: Int -> [Int] -> Int
cPCInt bitSize intArray = (\gamma -> gamma * complement gamma) $
    foldr
        (\column gamma ->
            let (onBits, offBits) = bimap length length $ partition id $ map (`testBit` column) intArray
            in  if onBits > offBits then gamma `setBit` column else gamma
        )
        0
        [0 .. bitSize - 1]
    where
        complement :: Int -> Int
        complement i = foldl' complementBit i [0 .. bitSize - 1]

calculateLifeSupportRating :: [[Bool]] -> Int
calculateLifeSupportRating matrix =
    let oxygenRating = findByOp (>) True (matrix, 0)
        scrubberRating = findByOp (<) False (matrix, 0)
    in  bitsToInt oxygenRating * bitsToInt scrubberRating
    where
        findByOp :: (Int -> Int -> Bool) -> Bool -> ([[Bool]], Int) -> [Bool]
        findByOp op tieBreaker (filteredMatrix, currentColumn)
            | length filteredMatrix == 1 = head filteredMatrix
            | otherwise =
                let columnToProcess = map (!! currentColumn) filteredMatrix
                    (onCount, offCount) = bimap length length $ partition id columnToProcess
                    desiredColumnVal = if
                        | onCount == offCount -> tieBreaker
                        | onCount `op` offCount -> True
                        | otherwise -> False
                    newMatrix = filter (\bits -> bits !! currentColumn == desiredColumnVal) filteredMatrix
                in findByOp op tieBreaker (newMatrix, currentColumn + 1)

cLSRInt :: Int -> [Int] -> Int
cLSRInt bitSize intArray =
    let oxygenRating = findByOp (>) True (intArray, bitSize - 1)
        scrubberRating = findByOp (<) False (intArray, bitSize - 1)
    in  oxygenRating * scrubberRating
    where
        findByOp :: (Int -> Int -> Bool) -> Bool -> ([Int], Int) -> Int
        findByOp op tieBreaker (remainingRows, currentColumn)
            | length remainingRows == 1 = head remainingRows
            | otherwise =
                let (onBits, offBits) =
                        bimap length length
                            $ partition id
                            $ map (`testBit` currentColumn) remainingRows
                    desiredColumnVal =
                        if onBits == offBits
                            then tieBreaker
                            else onBits `op` offBits
                    newRows = filter ((== desiredColumnVal) . flip testBit currentColumn) remainingRows
                in findByOp op tieBreaker (newRows, currentColumn - 1)

bitsToInt :: [Bool] -> Int
bitsToInt =
    foldl'
        (\i b ->
            let bitVal = if b then 1 else 0 in
            shiftL i 1 .|. bitVal
        )
        0
