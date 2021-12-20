{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day20 where

import Control.Arrow    ((&&&))
import Control.Monad
import Data.Array       (Array)
import Data.Bifunctor
import Data.Bits
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L


-- pt 2 takes a while, probably a better way like tracking only the edge
-- sequences and just using counts for the inside sequences?
--
-- Probably faster if we use grid of Int instead of Char as well.
--
-- But I'm on vacation & probably won't do either of those, lol
main :: IO ()
main = getInputAndSolve (parseInputRaw parseInputImage) (countOns 2) (countOns 50)


-- SOLVE

countOns :: Int -> InputImage -> Int
countOns steps ii@InputImage{..} =
    let finalImage = foldr (runStep ii) iiImage [1 .. steps]
    in  length . filter (== '#') . A.elems $ fromImage finalImage


-- HELPERS

runStep :: InputImage -> Int -> Image -> Image
runStep ii step initialImage =
    let
        -- base of new image
        expanded :: Image
        expanded = expandImage ii step initialImage
        expandedEls :: [((Int, Int), Char)]
        expandedEls =
            map (id &&& getEnhancementChar ii step expanded) $ A.indices $ fromImage expanded
    in  Image $ A.array (A.bounds $ fromImage expanded) expandedEls

newtype Image =
    Image
        { fromImage :: Array (Int, Int) Char
        } deriving (Eq)

instance Show Image where
    show = ("\n" <>) . A.showGridWith (: []) . fromImage

expandImage :: InputImage -> Int -> Image -> Image
expandImage inputImage step Image{..} =
    Image $ A.sparseGrid blankChar_ bumpedPixels
    where
        blankChar_ = blankChar inputImage step
        initialBounds :: (Int, Int)
        initialBounds = snd $ A.bounds fromImage
        newBounds :: (Int, Int)
        newBounds = bimap (+ 2) (+ 2) initialBounds
        bumpedPixels :: [((Int, Int), Char)]
        bumpedPixels =
            ((newBounds, blankChar_) :)
                . map (first (bimap succ succ))
                $ A.assocs fromImage

getEnhancementChar :: InputImage -> Int -> Image -> (Int, Int) -> Char
getEnhancementChar inputImage step (fromImage -> image) (x, y) =
    let surroundings =
            [ (x + dx, y + dy)
            | dy <- [-1 .. 1]
            , dx <- [-1 .. 1]
            ]
        ((minX, minY), (maxX, maxY)) = A.bounds image
        enhancementIndex =
            L.foldl'
                (\acc ix@(bx, by) ->
                    let b =
                            if bx < minX || bx > maxX || by < minY || by > maxY then
                                blankChar inputImage step
                            else
                                image A.! ix
                    in shiftL acc 1 .|. charToBit b
                )
                0
                surroundings
    in iiIndex inputImage A.! enhancementIndex

blankChar :: InputImage -> Int -> Char
blankChar InputImage{..} step
    | step `mod` 2 == 1 && iiIndex A.! 0 /= '.' =  '#'
    | otherwise = '.'

charToBit :: Char -> Int
charToBit = \case
    '.' -> 0
    '#' -> 1
    e -> error $ "unexpected char: " <> show e


-- PARSE

data InputImage =
    InputImage
        { iiIndex :: !(Array Int Char)
        , iiImage :: !Image
        } deriving (Show, Eq)

parseInputImage :: ReadP InputImage
parseInputImage = do
    chars <- count 512 get
    replicateM_ 2 newline
    let iiIndex = A.array (0, 511) $ zip [0 ..] chars
    iiImage <- Image <$> parseCharGrid (`elem` ".#")
    void newline
    return InputImage{..}
