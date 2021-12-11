#!/usr/bin/env bash
#
# ./mk-day.sh 08

DAY="${1}"

if [ -z "${DAY}" ]; then
    echo -e "day arg required:\n\tmake new-day day=09"
    exit 1
fi

touch "inputs/day${DAY}.txt"

cat >> "Day${DAY}.hs" <<EOF
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day${DAY} where

import Control.Monad
import Data.Array   (Array)
import Data.Bifunctor
import Data.Char
import Data.Either
import Data.Map     (Map)
import Data.Maybe
import Data.Set     (Set)
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
main = getInputAndSolve (const "Implement Parser") (const "Implement") (const "Implement")


-- PARSE



-- SOLVE



-- HELPERS

EOF
