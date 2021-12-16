{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day15 where

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
main = getInputAndSolve (parseInputRaw parseGrid) lowestTotalRisk fullLowestTotalRisk


-- SOLVE

lowestTotalRisk :: Grid -> Int
lowestTotalRisk =
    shortestPath

-- | This is really slow! Maybe a priority queue would help, but not ready
-- to implement that from scratch, lol.
fullLowestTotalRisk :: Grid -> Int
fullLowestTotalRisk Grid{..} =
    shortestPath . Grid $ A.array ((0, 0), newBounds) newElements
    where
        newElements :: [((Int, Int), Int)]
        newElements =
            L.foldr
                (\nextPos els ->
                    let s@(col, row) = nextPos `divMod` 5
                        incrementFactor = col + row
                        newEls = map
                            (bimap (bumpIndex s) (incrementBy incrementFactor))
                            initialElements
                    in  newEls <> els
                )
                initialElements
                [1 .. 24]
        newBounds :: (Int, Int)
        newBounds = bimap bumpBounds bumpBounds . snd $ A.bounds gGrid
        bumpBounds :: Int -> Int
        bumpBounds x = ((x + 1) * 5) - 1
        bumpIndex :: (Int, Int) -> (Int, Int) -> (Int, Int)
        bumpIndex (col, row) (x, y) =
            ( x + col * (initialWidth + 1)
            , y + row * (initialHeight + 1)
            )
        initialElements :: [((Int, Int), Int)]
        initialElements =
            A.assocs gGrid
        initialWidth :: Int
        initialWidth = fst . snd $ A.bounds gGrid
        initialHeight :: Int
        initialHeight = snd . snd $ A.bounds gGrid
        incrementBy :: Int -> Int -> Int
        incrementBy n v =
            case n of
                0 -> v
                _ -> incrementBy (n - 1) $ increment v
        increment :: Int -> Int
        increment v =
            if v == 9 then
                1
            else
                succ v




-- HELPERS

shortestPath :: Grid -> Int
shortestPath Grid{..} =
    recurse initialVisited initialDistances (0, 0)
    where
        initialVisited :: Array (Int, Int) Bool
        initialVisited =
            A.array (A.bounds gGrid) $ zip (A.indices gGrid) (repeat False)
        initialDistances :: Array (Int, Int) Int
        initialDistances =
            A.array (A.bounds gGrid) $ zip (A.indices gGrid) (repeat maxBound) <> [((0, 0), 0)]
        recurse :: Array (Int, Int) Bool -> Array (Int, Int) Int -> (Int, Int) -> Int
        recurse visited distances p =
            let neighbors =
                    filter (not . (visited A.!)) $ A.getGridNeighborsCardinal gGrid p
                distanceToP = distances A.! p
                newDistances =
                    foldr
                        (\neighbor newDist ->
                            A.accum
                                (\d _ ->
                                    if d == maxBound then
                                        distanceToP + gGrid A.! neighbor
                                    else
                                        min d (distanceToP + gGrid A.! neighbor)
                                )
                                newDist [(neighbor, ())]
                        )
                        distances
                        neighbors
                newVisited =
                    A.accum (\_ _ -> True) visited [(p, True)]
                minUnvisitedDistance =
                    foldr
                        (\(pos, dist) mbMinPos ->
                            case mbMinPos of
                                Nothing ->
                                    Just (pos, dist)
                                m@(Just (_, minDist)) ->
                                    if dist < minDist && not (newVisited A.! pos) then
                                        Just (pos, dist)
                                    else
                                        m
                        )
                        Nothing
                        (A.assocs newDistances)
            in  if newVisited A.! destination then
                    newDistances A.! destination
                else
                    recurse newVisited newDistances $ fst $ fromJust minUnvisitedDistance
        destination :: (Int, Int)
        destination = snd $ A.bounds gGrid


-- PARSE

newtype Grid =
    Grid
        { gGrid :: Array (Int, Int) Int
        } deriving (Eq, Ord)

instance Show Grid where
    show = A.showGrid . gGrid

parseGrid :: ReadP Grid
parseGrid = Grid <$> parseIntGrid
