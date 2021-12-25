{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day25 where

import Control.Arrow    ((&&&))
import Control.Monad
import Data.Array       (Array)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Function    (on)
import Data.Functor
import Data.Map         (Map)
import Data.Maybe
import Data.Set         (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Debug.Trace


-- (parseInput lineParser) OR (parseInputRaw fullInputParser)
main :: IO ()
main =
    getInputAndSolve (parseInputRaw parseGrid) countStepsToStop (const "Implement Part 2")


-- SOLVE


countStepsToStop :: Grid -> Int
countStepsToStop =
    untilStopped 1
    where
        untilStopped :: Int -> Grid -> Int
        untilStopped stepCount grid =
            let newGrid = runStep grid
            in  if newGrid == grid then
                    stepCount
                else
                    untilStopped (succ stepCount) newGrid



-- HELPERS

runStep :: Grid -> Grid
runStep Grid{fromGrid=g} =
    let eastMoved = moveEast validEastMoves g
        southMoved = moveSouth (validSouthMoves eastMoved) eastMoved
    in Grid southMoved
    where
        gridBounds :: (Int, Int)
        gridBounds = snd $ A.bounds g

        validEastMoves :: [(Int, Int)]
        validEastMoves =
            filter eastIsFree
                . map fst
                . filter ((== '>') . snd)
                $ A.assocs g
        eastIsFree :: (Int, Int) -> Bool
        eastIsFree (sourceX, sourceY) =
            let destY = sourceY
                destX = shiftRight sourceX
            in '.' == g A.! (destX, destY)
        moveEast :: [(Int, Int)] -> Array (Int, Int) Char -> Array (Int, Int) Char
        moveEast sources grid =
            let eastUpdates = concatMap
                    (\(sX, sY) ->
                        [((sX, sY), '.')
                        , ((shiftRight sX, sY), '>')
                        ]
                    )
                    sources
            in A.set eastUpdates grid

        validSouthMoves :: Array (Int, Int) Char -> [(Int, Int)]
        validSouthMoves grid =
            filter (southIsFree grid)
                . map fst
                . filter ((== 'v') . snd)
                $ A.assocs grid
        southIsFree :: Array (Int, Int) Char -> (Int, Int) -> Bool
        southIsFree grid (sX, sY) =
            let dY = shiftDown sY
            in '.' == grid A.! (sX, dY)

        moveSouth :: [(Int, Int)] -> Array (Int, Int) Char -> Array (Int, Int) Char
        moveSouth sources grid =
            let southUpdates = concatMap
                    (\(sX, sY) ->
                        [ ((sX, sY), '.')
                        , ((sX, shiftDown sY), 'v')
                        ]
                    )
                    sources
            in  A.set southUpdates grid

        shiftRight :: Int -> Int
        shiftRight x =
            if x == fst gridBounds then
                0
            else
                succ x

        shiftDown :: Int -> Int
        shiftDown y =
            if y == snd gridBounds then
                0
            else
                succ y






-- PARSE

newtype Grid =
    Grid
        { fromGrid :: Array (Int, Int) Char
        } deriving (Eq)

instance Show Grid where
    show = ("\n" <>) . A.showGridWith (: []) . fromGrid

parseGrid :: ReadP Grid
parseGrid =
    Grid <$> parseCharGrid (`elem` ".>v") <* newline
