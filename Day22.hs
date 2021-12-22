{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day22 where

import Control.Arrow    ((&&&))
import Control.Monad
import Data.Array       (Array)
import Data.Bifunctor
import Data.Bits
import Data.Char
import Data.Either
import Data.Function    (on)
import Data.Functor
import Data.Int         (Int64)
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


main :: IO ()
main = getInputAndSolve (parseInput parseInstruction) (countOnWithinRange (-50, 50)) countDiffs


-- SOLVE

countOnWithinRange :: (Int, Int) -> [Instruction] -> Int
countOnWithinRange (minDimension, maxDimension) is =
    length . filter snd $ A.assocs $ applyInstructions $ map limitInstruction $ filter (not . outOfRange) is
    where
        outOfRange :: Instruction -> Bool
        outOfRange Instruction{..} =
            iXMax <= minDimension ||
            iXMin >= maxDimension ||
            iYMax <= minDimension ||
            iYMin >= maxDimension ||
            iZMax <= minDimension ||
            iZMin >= maxDimension
        limitInstruction :: Instruction -> Instruction
        limitInstruction Instruction{..} =
            Instruction
                { iSetTo
                , iXMin = limit iXMin
                , iXMax = limit iXMax
                , iYMin = limit iYMin
                , iYMax = limit iYMax
                , iZMin = limit iZMin
                , iZMax = limit iZMax
                }
        limit :: Int -> Int
        limit x = min maxDimension (max minDimension x)

countDiffs :: [Instruction] -> Int
countDiffs =
    sum . map toSignedVolume . reverse . L.foldl' doInstruction []
    where
        -- When setting on, add negative overlapping cube + new cube
        -- When setting off, add negative overlapping cube
        doInstruction :: [(Int, Cube)] -> Instruction -> [(Int, Cube)]
        doInstruction initialCubes i =
            let instructionCube = cubeFromInstruction i
                instructionSign = if iSetTo i then 1 else (-1)
                diffs =
                    mapMaybe
                        (\(s, c) ->
                            let diffSign =
                                    if s == instructionSign then
                                        negate instructionSign
                                    else
                                        instructionSign
                            in (diffSign ,)
                                    <$> intersectingCube c instructionCube
                        )
                        initialCubes
            in  if instructionSign == 1 then
                    (instructionSign, instructionCube) : diffs <> initialCubes
                else
                    diffs <> initialCubes
        toSignedVolume :: (Int, Cube) -> Int
        toSignedVolume (s, c) = s * cubeVolume c


-- HELPERS

applyInstructions :: [Instruction] -> Array (Int, Int, Int) Bool
applyInstructions is =
    let (minX, maxX) = minimum &&& maximum $ concatMap (\i -> [iXMin i, iXMax i]) is
        (minY, maxY) = minimum &&& maximum $ concatMap (\i -> [iYMin i, iYMax i]) is
        (minZ, maxZ) = minimum &&& maximum $ concatMap (\i -> [iZMin i, iZMax i]) is
        bounds = ((minX, minY, minZ), (maxX, maxY, maxZ))
        initialReactor = A.array bounds . map (, False) $ A.range bounds
    in  L.foldl' applyInstruction initialReactor is
    where
        applyInstruction :: Array (Int, Int, Int) Bool -> Instruction -> Array (Int, Int, Int) Bool
        applyInstruction reactor Instruction{..} =
            A.setAll iSetTo (A.range ((iXMin, iYMin,iZMin), (iXMax, iYMax, iZMax))) reactor

data Cube =
    Cube
        { cX :: !(Int, Int)
        , cY :: !(Int, Int)
        , cZ :: !(Int, Int)
        } deriving (Show, Read, Eq, Ord)

cubeFromInstruction :: Instruction -> Cube
cubeFromInstruction Instruction{..} =
    Cube
        { cX = (iXMin, iXMax)
        , cY = (iYMin, iYMax)
        , cZ = (iZMin, iZMax)
        }

cubeVolume :: Cube -> Int
cubeVolume Cube{cX=(cx0, cx1), cY=(cy0, cy1), cZ=(cz0, cz1)} =
    (cx1 - cx0 + 1) * (cy1 - cy0 + 1) * (cz1 - cz0 + 1)

intersectingCube :: Cube -> Cube -> Maybe Cube
intersectingCube c1 c2 =
    let
        Cube{cX=c1x@(c1x0, c1x1), cY=c1y@(c1y0, c1y1), cZ=(c1z0, c1z1)} = c1
        Cube{cX=(c2x0, c2x1), cY=c2y@(c2y0, c2y1), cZ=c2z@(c2z0, c2z1)} = c2
    in
        zeroCube
            Cube
                { cX = (max c1x0 c2x0, min c1x1 c2x1)
                , cY = (max c1y0 c2y0, min c1y1 c2y1)
                , cZ = (max c1z0 c2z0, min c1z1 c2z1)
                }
    where
        zeroCube c@Cube{..}
            | uncurry (>) cX || uncurry (>) cY || uncurry (>) cZ = Nothing
            | otherwise = Just c


-- PARSE

data Instruction =
    Instruction
        { iSetTo :: !Bool
        , iXMin :: !Int
        , iXMax :: !Int
        , iYMin :: !Int
        , iYMax :: !Int
        , iZMin :: !Int
        , iZMax :: !Int
        } deriving (Show, Read, Eq, Ord)

parseInstruction :: ReadP Instruction
parseInstruction = do
    iSetTo <- choice [string "on" $> True, string "off" $> False]
    skipSpaces
    (iXMin, iXMax) <- parseMinMax "x"
    (iYMin, iYMax) <- parseMinMax "y"
    (iZMin, iZMax) <- parseMinMax "z"
    return Instruction{..}
    where
        parseMinMax :: String -> ReadP (Int, Int)
        parseMinMax leader = do
            void $ string $ leader <> "="
            x <- parseInt
            void $ string ".."
            y <- parseInt
            optional $ char ','
            return (x, y)
