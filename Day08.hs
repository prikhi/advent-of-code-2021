{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Day08 where

import qualified Data.List as L
import Data.Maybe
import Control.Monad
import Data.Char (isAlpha)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper


main :: IO ()
main = getInputAndSolve (parseInput parseDisplayLine) countUniqueOutputValues decodeAllSignals

data DigitDisplay =
    DigitDisplay
        { ddUniquePatterns :: [String]
        , ddOutputValue :: [String]
        } deriving (Show, Read, Eq)

parseDisplayLine :: ReadP DigitDisplay
parseDisplayLine = do
    ddUniquePatterns <- sepBy (many1 $ satisfy isAlpha) (char ' ')
    void $ string " | "
    ddOutputValue <- sepBy (many1 $ satisfy isAlpha) (char ' ')
    return DigitDisplay {..}

uniqueDigitCounts :: [Int]
uniqueDigitCounts = [2, 4, 3, 7]

countUniqueOutputValues :: [DigitDisplay] -> Int
countUniqueOutputValues dds =
    length . filter ((`elem` uniqueDigitCounts) . length) $ concatMap ddOutputValue dds

decodeAllSignals :: [DigitDisplay] -> Int
decodeAllSignals = sum . map decodeSignalOutput

{-|
map placements to ints:

 0
1 2
 3
4 5
 6

unique values:

1 has 2 signals (ix 2, 5)
4 has 4 signals (ix 1, 2, 3, 5)
7 has 3 signals (ix 0, 2, 5)
8 has 7 signals (ix 0, 1, 2, 3, 4, 5, 6)

other ix sets:

0 - 0, 1, 2, 4, 5, 6
2 - 0, 2, 3, 4, 6
3 - 0, 2, 3, 5, 6
5 - 0, 1, 3, 5, 6
6 - 0, 1, 3, 4, 5, 6
9 - 0, 1, 2, 3, 5, 6

So if we see something like:

acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab

we know the following signal sets match these numbers
- ab is 1
- dab is 7
- eafb is 4
- acedgfb is 8

then we know some signals possibilies
d is 0
a is 2 or 5
b is 2 or 5
e is 1 or 3
f is 1 or 3
c is 4 or 6
g is 4 or 6

then

find output w/ defcg + either b or a - that extra signal is signal 5 & the other is signal 2
find output w/ dabef + either c or g - that extra signal is signal 6 & the other is signal 4
find output w/ dabcg + either e or f - that extra signal is signal 1 & the other is signal 3

now w/ mapping from signal char -> ix, transform output displays into sorted [int] signals and create numbers

-}
decodeSignalOutput :: DigitDisplay -> Int
decodeSignalOutput DigitDisplay{..} =
    let -- find pair groups
        sigTwoOrFive  = fromJust $ L.find ((== 2) . length) ddUniquePatterns
        [signalZero]  =  fromJust (L.find ((== 3) . length) ddUniquePatterns) L.\\ sigTwoOrFive
        sigOneOrThree = fromJust (L.find ((== 4) . length) ddUniquePatterns) L.\\ sigTwoOrFive
        sigFourOrSix  = fromJust (L.find ((== 7) . length) ddUniquePatterns) L.\\ (signalZero : sigOneOrThree <> sigTwoOrFive)

        -- use 6-signal patterns to drill down pairs into single signals
        findSixDigitSignal otherSigs =
            fromJust (L.find (\p -> length p == 6 && null (otherSigs L.\\ p)) ddUniquePatterns) L.\\ otherSigs
        [signalFive]  = findSixDigitSignal $ signalZero : sigOneOrThree <> sigFourOrSix
        [signalTwo]   = sigTwoOrFive L.\\ [signalFive]
        [signalSix]   = findSixDigitSignal $ signalZero : sigOneOrThree <> sigTwoOrFive
        [signalFour]  = sigFourOrSix L.\\ [signalSix]
        [signalOne]   = findSixDigitSignal $ signalZero : sigTwoOrFive <> sigFourOrSix
        [signalThree] = sigOneOrThree L.\\ [signalOne]

        -- use found signals to convert chars into int segment positions
        convertChar c = if
            | c == signalZero -> 0
            | c == signalOne -> 1
            | c == signalTwo -> 2
            | c == signalThree -> 3
            | c == signalFour -> 4
            | c == signalFive -> 5
            | c == signalSix -> 6
            | otherwise -> error $ "Unexpected signal char: " <> [c]
    in digitsToInt $ map (intSignalsToDigit . map convertChar) ddOutputValue
    where
        digitsToInt :: [Int] -> Int
        digitsToInt = L.foldl' (\acc d -> acc * 10 + d) 0


intSignalsToDigit :: [Int] -> Int
intSignalsToDigit is = case L.sort is of
    [0, 1, 2, 4, 5, 6] -> 0
    [2, 5] -> 1
    [0, 2, 3, 4, 6] -> 2
    [0, 2, 3, 5, 6] -> 3
    [1, 2, 3, 5] -> 4
    [0, 1, 3, 5, 6] -> 5
    [0, 1, 3, 4, 5, 6] -> 6
    [0, 2, 5] -> 7
    [0, 1, 2, 3, 4, 5, 6] -> 8
    [0, 1, 2, 3, 5, 6] -> 9
    e -> error $ "Unexpected signal array: " <> show e
