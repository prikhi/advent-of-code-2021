{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad (void)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified GHC.Arr as A


main :: IO ()
main =
    getInputAndSolve (parseInput parseLine) (findOverlaps lineIsStraight) (findOverlaps $ const True)
    where
        lineIsStraight Line{..} =
            fst lEnd == fst lStart || snd lEnd == snd lStart


findOverlaps :: (Line -> Bool) -> [Line] -> Int
findOverlaps lineFilter (filter lineFilter -> ls) =
    let
        maxX =
            maximum $ concatMap (\Line{..} -> [fst lStart, fst lEnd]) ls
        maxY =
            maximum $ concatMap (\Line{..} -> [snd lStart, snd lEnd]) ls
        emptyGrid =
            A.listArray ((0, 0), (maxX, maxY))
                $ replicate @Int (succ maxX * succ maxY) 0
        filledGrid =
            foldr
                (\l arr ->
                    A.accum (+) arr $ zip (toPoints l) $ repeat 1
                )
                emptyGrid
                ls
    in A.foldrElems' (\i c -> if i >= 2 then succ c else c) 0 filledGrid
    where
        toPoints :: Line -> [(Int, Int)]
        toPoints Line{..}
            | fst lStart == fst lEnd =
                [(fst lStart, y) | y <- [snd left .. snd right]]
            | snd lStart == snd lEnd =
                [(x, snd lStart) | x <- [fst left .. fst right]]
            | otherwise =
                let xDirection = getDirection fst
                    yDirection = getDirection snd
                in
                    zip
                        [fst lStart, fst lStart + xDirection .. fst lEnd]
                        [snd lStart, snd lStart + yDirection .. snd lEnd]
            where
                (left, right) =
                    if lStart < lEnd
                        then (lStart, lEnd)
                        else (lEnd, lStart)
                getDirection :: ((Int, Int) -> Int) -> Int
                getDirection sel =
                    let diff = (sel lEnd - sel lStart)
                    in diff `div` abs diff


data Line
    = Line
        { lStart :: (Int, Int)
        , lEnd :: (Int, Int)
        } deriving (Show, Read, Eq, Ord)

parseLine :: ReadP Line
parseLine = do
    lStart <- parsePair
    void $ string " -> "
    lEnd <- parsePair
    return Line {..}
    where
        parsePair :: ReadP (Int, Int)
        parsePair = do
            x <- parseInt
            void $ char ','
            y <- parseInt
            return (x, y)
