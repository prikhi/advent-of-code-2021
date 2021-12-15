{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day14 where

import Control.Monad
import Data.Char
import Data.Map         (Map)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified Data.Map as M


main :: IO ()
main = getInputAndSolve (parseInputRaw parsePolymerization) (mostLeastDiff 10) (mostLeastDiffByPair 40)


-- SOLVE

mostLeastDiff :: Int -> Polymerization -> Int
mostLeastDiff steps Polymerization{..} =
    let finalPolymer =
            L.foldl'
                (\polymer _ ->
                    polymerize pInsertions polymer
                )
                pTemplate
                [1 .. steps]
        (leastCommon, mostCommon) =
            leastAndMostCommon $ L.foldr succKey M.empty finalPolymer
    in  mostCommon - leastCommon

mostLeastDiffByPair :: Int -> Polymerization -> Int
mostLeastDiffByPair steps Polymerization{..} =
    let (_, initialPairs) =
            L.foldl'
                (\(mbPrevChar, acc) nextChar ->
                    case mbPrevChar of
                        Nothing ->
                            (Just nextChar, acc)
                        Just prevChar ->
                            (Just nextChar, succKey [prevChar, nextChar] acc)
                )
                (Nothing, M.empty)
                pTemplate
        initialCounts =
            L.foldr succKey M.empty pTemplate
        (_, finalCounts) =
            L.foldl'
                (\(pairs, counts) _ ->
                    polymerizeViaPairCounts pInsertions pairs counts
                )
                (initialPairs, initialCounts)
                [1 .. steps]
        (leastCommon, mostCommon) =
            leastAndMostCommon finalCounts
    in  mostCommon - leastCommon


-- HELPERS

polymerize :: Map String Char -> String -> String
polymerize insertions =
    snd . L.foldr
        (\prevChar (mbNextChar, result) ->
            case mbNextChar of
                Nothing ->
                    (Just prevChar, prevChar : result)
                Just nextChar ->
                    let pair = [prevChar, nextChar]
                        newResult = case M.lookup pair insertions of
                            Nothing ->
                                prevChar : result
                            Just insertion ->
                                prevChar : insertion : result
                    in  (Just prevChar, newResult)
        )
        (Nothing, "")

polymerizeViaPairCounts :: Map String Char -> Map String Int -> Map Char Int -> (Map String Int, Map Char Int)
polymerizeViaPairCounts insertions initialPairs initialCounts =
    M.foldrWithKeys
        (\p@[l, r] c (pairs, counts) ->
            case M.lookup p insertions of
                Nothing ->
                    ( addToKey c p pairs
                    , counts
                    )
                Just new ->
                    ( addToKey c [l, new] $ addToKey c [new, r] pairs
                    , addToKey c new counts
                    )
        )
        (M.empty, initialCounts)
        initialPairs

leastAndMostCommon :: Map Char Int -> (Int, Int)
leastAndMostCommon =
    foldr
        (\c (least, most) ->
            ( if c < least then c else least
            , if c > most then c else most
            )
        )
        (maxBound, minBound)

-- | Increase the key's value by 1
succKey :: Ord k => k -> Map k Int -> Map k Int
succKey = addToKey 1

-- | Add the given number to the key's value, or set it if not in the Map.
addToKey :: Ord k => Int -> k -> Map k Int -> Map k Int
addToKey v = M.upsert (maybe v (+ v))


-- PARSE

data Polymerization =
    Polymerization
        { pTemplate :: !String
        , pInsertions :: !(Map String Char)
        } deriving (Show, Read, Eq, Ord)

parsePolymerization :: ReadP Polymerization
parsePolymerization = do
    pTemplate <- munch1 isAlpha <* replicateM_ 2 newline
    pInsertions <- fmap M.fromList . flip sepBy1 newline $ do
        adjacent <- munch1 isAlpha <* string " -> "
        insertion <- satisfy isAlpha
        return (adjacent, insertion)
    void newline
    return Polymerization{..}
