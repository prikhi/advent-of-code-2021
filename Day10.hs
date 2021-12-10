{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Day10 where

import Control.Monad
import Data.Either

import Harness

import qualified Data.List as L


main :: IO ()
main = getInputAndSolve (map checkBalanced . lines) getSyntaxErrorScore getAutocompleteScore

getSyntaxErrorScore :: [Either Char [Char]] -> Int
getSyntaxErrorScore =
    sum . map scoreIllegalChar . fst . partitionEithers
    where
        scoreIllegalChar :: Char -> Int
        scoreIllegalChar = \case
            ')' -> 3
            ']' -> 57
            '}' -> 1197
            '>' -> 25137
            c -> error $ "Unexpected char to score: " <> [c]

getAutocompleteScore :: [Either Char [Char]] -> Int
getAutocompleteScore es =
    let scores =
            L.sort
                . map scoreRemainingChars
                . snd
                $ partitionEithers es
    in scores !! (length scores `div` 2)
    where
        scoreRemainingChars :: [Char] -> Int
        scoreRemainingChars =
            L.foldl'
                (\score c ->
                    score * 5 + scoreAutocompleteChar (matchingClose c)
                )
                0
        scoreAutocompleteChar :: Char -> Int
        scoreAutocompleteChar = \case
            ')' -> 1
            ']' -> 2
            '}' -> 3
            '>' -> 4
            c   -> error $ "Unexpected char to score: " <> [c]


-- Left  == invalid char
-- Right == remaining stack
checkBalanced :: String -> Either Char [Char]
checkBalanced =
    foldM
        (\stack c ->
            if isClose c then
                case stack of
                    top : restOfStack ->
                        if matchingClose top == c then
                            Right restOfStack
                        else
                            Left c
                    [] ->
                        Left c
            else
                Right $ c : stack
        )
        []
    where
        isClose :: Char -> Bool
        isClose = \case
            ']' -> True
            ')' -> True
            '}' -> True
            '>' -> True
            _   -> False

matchingClose :: Char -> Char
matchingClose = \case
    '[' -> ']'
    '(' -> ')'
    '{' -> '}'
    '<' -> '>'
    c   -> error $ "Unexpected opening char: " <> [c]
