#!/usr/bin/env bash
#
# ./mk-day.sh 08

DAY="${1}"

touch "inputs/day${DAY}.txt"

cat >> "Day${DAY}.hs" <<EOF
module Day${DAY} where

import Control.Monad
import Data.Maybe
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified GHC.Arr as A


-- getInputAndSolve (parseInput lineParser) part1 part2
-- getInputAndSolve (parseInputRaw fullParser) part1 part2
main :: IO ()
main = putStrLn "Hello World"
EOF
