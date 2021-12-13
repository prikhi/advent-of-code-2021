{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day13 where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L


main :: IO ()
main = getInputAndSolve (parseInputRaw parsePuzzle) foldOne foldAll


-- SOLVE

foldOne :: Puzzle -> Int
foldOne Puzzle{..} =
    length . foldPaper pDots $ head pFolds

foldAll :: Puzzle -> PrettyOutput
foldAll Puzzle{..} =
    render $ L.foldl' foldPaper pDots pFolds
    where
        -- | Render the dots as a grid of Chars
        render :: [(Int, Int)] -> PrettyOutput
        render dots =
            PrettyOutput
                . A.showGridWith (: [])
                . A.sparseGrid ' '
                $ map (, '#') dots


-- HELPERS

foldPaper :: [(Int, Int)] -> Fold -> [(Int, Int)]
foldPaper dots = L.nub . \case
    FoldX i ->
        foldLeft i dots
    FoldY i ->
        foldUp i dots

foldUp :: Int -> [(Int, Int)] -> [(Int, Int)]
foldUp line =
    map . second $ \y ->
        if y > line then
            2 * line - y
        else
            y

foldLeft :: Int -> [(Int, Int)] -> [(Int, Int)]
foldLeft line =
    map . first $ \x ->
        if x > line then
            2 * line - x
        else
            x

-- | Helper type to prevent quotes/escape-sequences from rendering when
-- a solver returns a String.
--
-- TODO: if useful for more exercises, move to Harness module.
newtype PrettyOutput = PrettyOutput String

-- | Render with leading newline + indentation.
instance Show PrettyOutput where
    show (PrettyOutput s) = ("\n" <>) $ unlines $ map ("\t" <>) $ lines s


-- PARSE

data Puzzle
    = Puzzle
        { pDots :: ![(Int, Int)]
        , pFolds :: ![Fold]
        } deriving (Show, Read, Eq, Ord)

data Fold
    = FoldX !Int
    | FoldY !Int
    deriving (Show, Read, Eq, Ord)

parsePuzzle :: ReadP Puzzle
parsePuzzle = do
    pDots <- sepBy parseDot newline
    replicateM_ 2 newline
    pFolds <- sepBy parseFold newline
    newline $> Puzzle{..}
    where
        parseDot :: ReadP (Int, Int)
        parseDot = do
            x <- parseInt
            void $ char ','
            y <- parseInt
            return (x, y)
        parseFold :: ReadP Fold
        parseFold =
            choice
                [ fmap FoldY $ string "fold along y=" *> parseInt
                , fmap FoldX $ string "fold along x=" *> parseInt
                ]
