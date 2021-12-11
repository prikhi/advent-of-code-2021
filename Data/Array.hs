module Data.Array
    ( module A
    , setAll
    , showGrid
    , getGridNeighborsCardinal
    , getGridNeighborsDiagonal
    ) where

import GHC.Arr as A


{-# INLINABLE setAll #-}
setAll :: (A.Ix i) => a -> [i] -> A.Array i a -> A.Array i a
setAll a is arr =
    A.accum (\_ x -> x) arr (zip is (repeat a))


-- GRID HELPERS

{-# INLINABLE showGrid #-}
-- | Render a grid line by line.
showGrid :: Show a => A.Array (Int, Int) a -> String
showGrid grid =
    let (_, (width, height)) = A.bounds grid
    in  unlines
            [ concat [ show (grid A.! (col, row)) | col <- [0 .. width] ]
            | row <- [0 .. height]
            ]

{-# INLINABLE getGridNeighborsCardinal #-}
-- | Get the neighbors in the cardinal directions.
getGridNeighborsCardinal :: A.Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
getGridNeighborsCardinal grid (x, y) =
    let surroundings =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , not $
                (dy == 0 && dx == 0) ||
                (abs dy == 1 && abs dx == 1)
            ]
        (_, (width, height)) = A.bounds grid
    in  filter
            (\(sx, sy) ->
                sx >= 0 && sx <= width && sy >= 0 && sy <= height
            )
            surroundings

{-# INLINABLE getGridNeighborsDiagonal #-}
-- | Get all neighbors, including the diagonals.
getGridNeighborsDiagonal :: A.Array (Int, Int) a -> (Int, Int) -> [(Int, Int)]
getGridNeighborsDiagonal grid (x, y) =
    let surroundings =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , not (dx == 0 && dy == 0)
            ]
        (_, (width, height)) = A.bounds grid
    in  filter
            (\(sx, sy) ->
                sx >= 0 && sx <= width && sy >= 0 && sy <= height
            )
            surroundings
