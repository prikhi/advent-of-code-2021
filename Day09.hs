{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day09 where

import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.Ord (Down(..))
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified Data.Set as S
import qualified GHC.Arr as A


main :: IO ()
main = getInputAndSolve (parseInputRaw parseHeightMap) riskTotal basinSize
    where
        parseHeightMap :: ReadP HeightMap
        parseHeightMap = do
            heightLines <- filter (not . null) <$> sepBy (many $ satisfy isDigit) newline
            let hmWidth = length heightLines - 1
                hmHeight = length (head heightLines) - 1
                hmGrid =
                    A.listArray ((0, 0), (hmWidth, hmHeight))
                        $ map (read . (: []))
                        $ concat heightLines
            return HeightMap {..}

data HeightMap =
    HeightMap
        { hmGrid :: A.Array (Int, Int) Int
        , hmHeight :: Int
        , hmWidth :: Int
        } deriving (Show, Read, Eq)

riskTotal :: HeightMap -> Int
riskTotal hm@HeightMap{..} =
    sum . map (\p -> hmGrid A.! p + 1) $ findLowPoints hm

findLowPoints :: HeightMap -> [(Int, Int)]
findLowPoints hm@HeightMap{..} =
    let allPositions = [ (x, y) | y <- [0 .. hmHeight], x <- [0 .. hmWidth] ]
    in filter isLowest allPositions
    where
        isLowest :: (Int, Int) -> Bool
        isLowest p =
            let surroundingPoints = getPointsAround hm p
                pointValue = hmGrid A.! p
            in  all (\surroundingPoint -> hmGrid A.! surroundingPoint > pointValue) surroundingPoints

basinSize :: HeightMap -> Int
basinSize hm@HeightMap{..} =
    let lowPoints = L.sortOn (hmGrid A.!) $ findLowPoints hm
        basins :: [[(Int, Int)]]
        basins =
            snd $ L.foldl'
                (\(c, acc) p ->
                    second (: acc) $ mkBasin c p
                )
                (S.empty, [])
                lowPoints
    in  product
            $ take 3
            $ map length
            $ L.sortOn (Down . length) basins
    where
        mkBasin :: S.Set (Int, Int) -> (Int, Int) -> (S.Set (Int, Int), [(Int, Int)])
        mkBasin claimed pos =
            if S.contains pos claimed then
                (claimed, [])
            else
                getInBasin claimed pos

        getInBasin :: S.Set (Int, Int) -> (Int, Int) -> (S.Set (Int, Int), [(Int, Int)])
        getInBasin claims pos =
            let surroundingPoints = getPointsAround hm pos
                pointsInBasin =
                    filter
                        (\surrPos ->
                            not (S.contains surrPos claims) && (hmGrid A.! surrPos) /= 9
                        ) surroundingPoints
                newClaims = foldr S.insert claims $ pos : pointsInBasin
                adjacentBasinPoints =
                    L.foldl'
                        (\(cs, ps) p ->
                            second (: ps) $ getInBasin cs p
                        )
                        (newClaims, [[pos]])
                        pointsInBasin
            in second concat adjacentBasinPoints

getPointsAround :: HeightMap -> (Int, Int) -> [(Int, Int)]
getPointsAround HeightMap{..} (x, y) =
    let surroundings =
            [ (x + dx, y + dy)
            | dx <- [-1 .. 1]
            , dy <- [-1 .. 1]
            , not $
                (dy == 0 && dx == 0) ||
                (abs dy == 1 && abs dx == 1)
            ]
    in  filter
            (\(sx, sy) ->
                sx >= 0 && sx <= hmWidth && sy >= 0 && sy <= hmHeight
            )
            surroundings
