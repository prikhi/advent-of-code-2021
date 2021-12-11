{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day11 where

import Control.Monad
import Data.Bifunctor
import Data.Either
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Set as S


main :: IO ()
main = getInputAndSolve (parseInputRaw parseGrid) (countFlashes 100) findCompleteFlash


newtype Grid =
    Grid
        { gGrid :: A.Array (Int, Int) Int
        }

instance Show Grid where
    show Grid{..} =
        A.showGrid gGrid

parseGrid :: ReadP Grid
parseGrid =
    Grid <$> parseIntGrid


countFlashes :: Int -> Grid -> Int
countFlashes steps initialGrid =
    snd $
        L.foldl'
            (\(grid, flashes) _ ->
                second (+ flashes) $ stepFlash grid
            )
            (initialGrid, 0)
            [1 .. steps]

findCompleteFlash :: Grid -> Int
findCompleteFlash initialGrid =
    let targetCount = A.numElements $ gGrid initialGrid
    in  fromLeft 0 $
            foldM
                (\grid step ->
                    let (newGrid, flashCount) = stepFlash grid
                    in if flashCount == targetCount then
                            Left step
                        else
                            Right newGrid
                )
                initialGrid
                [1 .. ]


stepFlash :: Grid -> (Grid, Int)
stepFlash initialGrid =
    let incrementedGrid =
            Grid $ A.amap (+ 1) $ gGrid initialGrid
        (flashed, flashedGrid) =
            L.foldl' runStepForPos (S.empty, incrementedGrid) $ A.indices (gGrid initialGrid)
        resetGrid =
            Grid $ A.setAll 0 (S.toList flashed) $ gGrid flashedGrid
    in  (resetGrid, length flashed)

runStepForPos :: (S.Set (Int, Int), Grid) -> (Int, Int) -> (S.Set (Int, Int), Grid)
runStepForPos (flashed, g) p =
    let neighbors = filter (not . (`S.contains` flashed)) $ A.getGridNeighborsDiagonal (gGrid g) p
        incrementedNeighbors = Grid $ A.accum (+) (gGrid g) $ zip neighbors $ repeat 1
    in  if gGrid g A.! p > 9 && not (S.contains p flashed) then
            L.foldl' runStepForPos
                (S.insert p flashed, incrementedNeighbors)
                neighbors
        else
            (flashed, g)
