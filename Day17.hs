{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day17 where

import Control.Monad
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper


main :: IO ()
main =
    getInputAndSolve
        (parseInputRaw parseTargetArea)
        findMaxHeight
        countPossibleVelocities


-- SOLVE

findMaxHeight :: TargetArea -> Int
findMaxHeight =
    maximum . map (maximum . map snd) . allPossiblePaths

countPossibleVelocities :: TargetArea -> Int
countPossibleVelocities =
    length . allPossiblePaths


-- HELPERS


allPossiblePaths :: TargetArea -> [[(Int, Int)]]
allPossiblePaths ta =
    let
        -- target finding is fast so we can use some really wide ranges
        vxRange = [1  .. maxX ta]
        vyRange = [minY ta .. abs (minY ta)]
    in
        mapMaybe
            (stepUntilFinished ta)
            [ (vx, vy) | vx <- vxRange, vy <- vyRange ]

stepUntilFinished :: TargetArea -> (Int, Int) -> Maybe [(Int, Int)]
stepUntilFinished ta initialVelocity =
    go [] ((0, 0), initialVelocity)
    where
        go :: [(Int, Int)] -> ((Int, Int), (Int, Int)) -> Maybe [(Int, Int)]
        go prev vec@(p, _)
            | probePastTargetArea p ta = Nothing
            | probeInTargetArea p ta = Just $ p : prev
            | otherwise = go (p:prev) $ stepProbe vec


stepProbe :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
stepProbe ((px, py), (vx, vy)) =
    ( ( px + vx
      , py + vy
      )
    , ( max 0 (vx - 1)
      , vy - 1
      )
    )

probePastTargetArea :: (Int, Int) -> TargetArea -> Bool
probePastTargetArea (x, y) TargetArea{..} =
    x > maxX || y < minY

probeInTargetArea :: (Int, Int) -> TargetArea -> Bool
probeInTargetArea (x, y) TargetArea{..} =
    x >= minX && x <= maxX && y >= minY && y <= maxY


-- PARSE

data TargetArea =
    TargetArea
        { minX :: !Int
        , maxX :: !Int
        , minY :: !Int
        , maxY :: !Int
        } deriving (Show)

parseTargetArea :: ReadP TargetArea
parseTargetArea = do
    void $ string "target area: x="
    minX <- parseInt
    void $ string ".."
    maxX <- parseInt
    void $ string ", y="
    minY <- parseInt
    void $ string ".."
    maxY <- parseInt
    void newline
    return TargetArea{..}
