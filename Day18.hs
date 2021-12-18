{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day18 where

import Control.Monad
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L


main :: IO ()
main = getInputAndSolve (parseInput parsePair) (calculateMagnitude . addNumbers) findLargestMagnitude


-- SOLVE

addNumbers :: [Pair] -> Pair
addNumbers =
    L.foldl1' addPair
    where
        addPair :: Pair -> Pair -> Pair
        addPair acc new = repeatedlyExplodeAndSplit $ Pair acc new

findLargestMagnitude :: [Pair] -> Int
findLargestMagnitude ps =
    maximum
        [ calculateMagnitude $ repeatedlyExplodeAndSplit (Pair x y)
        | x <- ps, y <- ps, x /= y
        ]


-- HELPERS

calculateMagnitude :: Pair -> Int
calculateMagnitude = \case
    Single x -> x
    Pair l r -> calculateMagnitude l * 3 + calculateMagnitude r * 2

repeatedlyExplodeAndSplit :: Pair -> Pair
repeatedlyExplodeAndSplit p
    | explodable p = repeatedlyExplodeAndSplit $ explode p
    | splittable p = repeatedlyExplodeAndSplit $ split p
    | otherwise = p

explodable :: Pair -> Bool
explodable =
    go 0
    where
        go :: Int -> Pair -> Bool
        go currentDepth = \case
            Pair (Single _) (Single _) -> currentDepth == 4
            Pair l r -> go (succ currentDepth) l || go (succ currentDepth) r
            Single _ -> False

splittable :: Pair -> Bool
splittable = \case
    Single x -> x >= 10
    Pair l r -> splittable l || splittable r

split :: Pair -> Pair
split = \case
    s@(Single x)
        | x >= 10 -> Pair (Single $ x `div` 2) (Single $ uncurry (+) $ x `divMod` 2)
        | otherwise -> s
    Pair l r ->
        let
            l_ = split l
            r_ = split r
        in
            if l /= l_ then
                Pair l_ r
            else
                Pair l r_

explode :: Pair -> Pair
explode = fst . go 0
    where
        go :: Int -> Pair -> (Pair, (Maybe Int, Maybe Int))
        go currentDepth = \case
            p@(Pair (Single l) (Single r))
                | currentDepth == 4 -> (Single 0, (Just l, Just r))
                | otherwise -> (p, (Nothing, Nothing))
            Pair l r ->
                let
                    (l_, (mbLL, mbLR)) = go (succ currentDepth) l
                    (r_, (mbRL, mbRR)) = go (succ currentDepth) r
                in
                    if l /= l_ then
                        (Pair l_ (maybe r (addToLeftmost r) mbLR), (mbLL, Nothing))
                    else
                        (Pair (maybe l (addToRightmost l) mbRL) r_, (Nothing, mbRR))
            s@(Single _) -> (s, (Nothing, Nothing))
        addToLeftmost :: Pair -> Int -> Pair
        addToLeftmost p i = case p of
            Single x -> Single $ x + i
            Pair l r -> Pair (addToLeftmost l i) r
        addToRightmost :: Pair -> Int -> Pair
        addToRightmost p i = case p of
            Single x -> Single $ x + i
            Pair l r -> Pair l (addToRightmost r i)


-- PARSE

data Pair
    = Pair !Pair !Pair
    | Single !Int
    deriving (Read, Eq)

instance Show Pair where
    show = \case
        Single x -> show x
        Pair l r -> "[" <> show l <> "," <> show r <> "]"

parsePair :: ReadP Pair
parsePair = do
    void $ char '['
    lPair <- choice [Single <$> parseInt, parsePair]
    void $ char ','
    rPair <- choice [Single <$> parseInt, parsePair]
    void $ char ']'
    return $ Pair lPair rPair
