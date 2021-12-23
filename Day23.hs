{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day23 where

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


main :: IO ()
main = getInputAndSolve (parseInputRaw parseLayout) id (const "Implement Part 2")


{-

22 days of programming, I think I earned a logic puzzle insted of
a programming puzzle, so I just did these by hand:

Pt1:

#############
#...........#
###A#D#A#B###
  #B#C#D#C#
  #########

#############
#A..........#
###.#D#A#B###
  #B#C#D#C#
  #########

#############
#AA.........#
###.#D#.#B###
  #B#C#D#C#
  #########

#############
#AA.......B.#
###.#D#.#.###
  #B#C#D#C#
  #########

#############
#AA...C...B.#
###.#D#.#.###
  #B#C#D#.#
  #########

#############
#AA...C...B.#
###.#D#.#.###
  #B#C#.#D#
  #########

#############
#AA.......B.#
###.#D#.#.###
  #B#C#C#D#
  #########

#############
#AA.......B.#
###.#.#.#D###
  #B#C#C#D#
  #########

#############
#AA.......B.#
###.#.#C#D###
  #B#.#C#D#
  #########

#############
#AA.......B.#
###.#.#C#D###
  #.#B#C#D#
  #########

#############
#AA.........#
###.#B#C#D###
  #.#B#C#D#
  #########

#############
#A..........#
###.#B#C#D###
  #A#B#C#D#
  #########

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########



Pt2:

#############
#...........#
###A#D#A#B###
  #D#C#B#A#
  #D#B#A#C#
  #B#C#D#C#
  #########

#############
#AB.B.D...AA#
###A#D#.#.###
  #D#C#.#.#
  #D#B#.#C#
  #B#C#.#C#
  #########

#############
#AB.B.....AA#
###A#.#.#.###
  #D#C#.#.#
  #D#B#C#D#
  #B#C#C#D#
  #########

#############
#AB.B...B.AA#
###A#.#C#.###
  #D#.#C#.#
  #D#.#C#D#
  #B#.#C#D#
  #########

#############
#AA.......AA#
###.#.#C#.###
  #D#B#C#.#
  #D#B#C#D#
  #B#B#C#D#
  #########

#############
#AA.......AA#
###.#B#C#D###
  #.#B#C#D#
  #.#B#C#D#
  #.#B#C#D#
  #########

#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########


-}

-- SOLVE


-- positions like this:
--
-- 12 3 4 5 67
--   8 9 0 1
--   2 3 4 5
--
-- ?
--
-- Make a graph of all possible states, weight is amount of energy needed
-- to reach that state. destination is final layout.
--
-- if A*, heuristic is distance of all aphipods to their desired lanes
--
-- Given a state, find all possible moves for all amphipod, add the states
-- to explore.
--
-- Track amphipod type & position.
--
-- Where track position? in Array would be nice for checking valid moves.
--
-- Need Set for visited states, Map for distances


-- HELPERS



-- PARSE

newtype Layout =
    Layout
        { lLanes :: [[Char]]
        } deriving (Show, Read, Eq, Ord)

parseLayout :: ReadP Layout
parseLayout = do
    parseWall <* newline
    parseWall <* newline
    parseWall
    firstLine <- sepBy1 (satisfy isAlpha) parseWall <* parseWall <* newline
    parseWall
    secondLine <- sepBy1 (satisfy isAlpha) parseWall <* parseWall <* newline
    let lLanes = L.transpose [firstLine, secondLine]
    parseWall
    void newline
    return Layout{..}
    where
        parseWall :: ReadP ()
        parseWall = void . many1 $ choice [char '#', char ' ', char '.']
