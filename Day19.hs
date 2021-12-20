{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Day19 where

import Control.Monad
import Data.Map         (Map)
import Data.Maybe
import Data.Set         (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


main :: IO ()
main = getInputAndSolve (parseInput parseScanner) countBeacons maxScannerDistance


-- SOLVE

countBeacons :: [Scanner] -> Int
countBeacons =
    length . S.toList . fst . getBeaconAndScannerPositions

maxScannerDistance :: [Scanner] -> Int
maxScannerDistance ss =
    let (_, M.toList -> scanners) = getBeaconAndScannerPositions ss
    in  maximum $ do
            (s1, (s1x, s1y, s1z)) <- scanners
            (s2, (s2x, s2y, s2z)) <- scanners
            guard $ s1 /= s2
            return $ abs (s1x - s2x) + abs (s1y - s2y) + abs (s1z - s2z)


-- HELPERS

getBeaconAndScannerPositions :: [Scanner] -> (Set Position, Map Int (Int, Int, Int))
getBeaconAndScannerPositions = \case
    [] -> error "NO SCANNERS!"
    initialScanner : ss ->
        locateBeaconsAndScanners
            [initialScanner]
            (S.fromList $ sBeacons initialScanner)
            (M.fromList [(0, (0, 0, 0))])
            ss
    where
        locateBeaconsAndScanners
            :: [Scanner]
            -- ^ Known scanners w/ rotated beacon positions
            -> Set Position
            -- ^ All known beacon positions
            -> Map Int (Int, Int, Int)
            -- ^ Known scanner positions
            -> [Scanner]
            -- ^ Unknow scanners
            -> (Set Position, Map Int (Int, Int, Int))
        locateBeaconsAndScanners locatedScanners locatedBeacons !scannerPositions scannersToProcess =
            if null scannersToProcess then
                (locatedBeacons, M.fromList $ M.toList scannerPositions)
            else
                let
                    -- find unprocessed scanner that overlaps known
                    knownId :: Int
                    overlappingScanner :: Scanner
                    overlappingPositions :: [Position]
                    (knownId, (overlappingScanner, overlappingPositions)) =
                        findOverlappingScanner locatedScanners scannersToProcess
                    knownScanner :: Scanner
                    knownScanner = fromJust $ L.find ((== knownId) . sId) locatedScanners
                    -- calc absolute position of new scanner
                    overlappingScannerAbsolutePos :: (Int, Int, Int)
                    overlappingScannerAbsolutePos =
                        calculateAbsoluteScannerPosition knownScanner overlappingPositions
                    -- calc absolute positions of the new scaner's beacons
                    newBeaconAbsolutePos :: [Position]
                    newBeaconAbsolutePos =
                        map (relativeToAbsolute overlappingScannerAbsolutePos) overlappingPositions
                in  locateBeaconsAndScanners
                        (overlappingScanner {sBeacons = newBeaconAbsolutePos} : locatedScanners)
                        (L.foldr S.insert locatedBeacons newBeaconAbsolutePos)
                        (M.insert (sId overlappingScanner) overlappingScannerAbsolutePos scannerPositions)
                        (filter (/= overlappingScanner) scannersToProcess)
        -- go through unknown scanners, finding one that has overlapping
        -- difference vectors of 12 points w/ a known scanner.
        --
        -- return ID of known scanner, the locatable unknown scanner & it's
        -- beacon positions, rotated to the perspective of scanner 0
        findOverlappingScanner :: [Scanner] -> [Scanner] -> (Int, (Scanner, [Position]))
        findOverlappingScanner known =  \case
            [] -> error "findOverlappingScanner: no remaining unknowns"
            unknown : rest ->
                let
                    unknownBeaconRotations :: [[Position]]
                    unknownBeaconRotations = map (map fst) $ sRotations unknown
                    checkOverlap :: Scanner -> Maybe (Int, [Position])
                    checkOverlap Scanner{..} =
                        let !knownVectorSet = positionVectorSet sBeacons
                            !vectorSetIntersection = concatMap
                                (\rotation ->
                                    [ rotation
                                    | k <- map snd knownVectorSet
                                    , r <- map snd $ positionVectorSet rotation
                                    , let intersection = k `L.intersect` r
                                    , length intersection >= 11
                                    ]
                                ) unknownBeaconRotations
                        in
                            case L.find (not . null) vectorSetIntersection of
                                Just rotatedUnknownVectorSet ->
                                    Just (sId, rotatedUnknownVectorSet)
                                _ ->
                                    Nothing
                in  case listToMaybe $ mapMaybe checkOverlap known of
                        Nothing ->
                            findOverlappingScanner known rest
                        Just (scanner, unknownVectors) ->
                            (scanner, (unknown, unknownVectors))
        -- given a known, re-oriented scanner & a set of beacons from the
        -- unknown scanner, calculate the unknown scanner's position
        -- relative to scanner 0.
        calculateAbsoluteScannerPosition :: Scanner -> [Position] -> (Int, Int, Int)
        calculateAbsoluteScannerPosition known rotated =
            let rotatedVectorSet = positionVectorSet rotated
                knownVectorSet = positionVectorSet $ sBeacons known
                commonPoint =
                    listToMaybe $ concatMap
                        (\(rotatedPt, rotatedVec) ->
                            mapMaybe
                                (\(knownPt, knownVec) ->
                                    if length (knownVec `L.intersect` rotatedVec) >= 11 then
                                        Just (knownPt, rotatedPt)
                                    else
                                        Nothing

                                )
                                knownVectorSet
                        )
                        rotatedVectorSet
            in
                case commonPoint of
                    Nothing ->
                        error "calculateAbsoluteScannerPosition: no commonPoint found"
                    Just (p1, p2) ->
                        ( pX p1 - pX p2
                        , pY p1 - pY p2
                        , pZ p1 - pZ p2
                        )
        -- given an scanner offset, convert relative positions to absolute
        -- positions.
        relativeToAbsolute :: (Int, Int, Int) -> Position -> Position
        relativeToAbsolute (dX, dY, dZ) Position{..} =
            Position
                { pX = pX + dX
                , pY = pY + dY
                , pZ = pZ + dZ
                }


-- | Given list of positions, calculate a sorted list containing point->point
-- magnitudes for all positions against every other position.
positionVectorSet :: [Position] -> [(Position, [(Int, Int, Int)])]
positionVectorSet ps =
    map (\p -> (p, calculatePointToPointVectors p)) ps
    where
        calculatePointToPointVectors :: Position -> [(Int, Int, Int)]
        calculatePointToPointVectors p =
            let otherPoints = filter (/= p) ps
            in  map (calculateVector p) otherPoints
        calculateVector :: Position -> Position -> (Int, Int, Int)
        calculateVector p1 p2 =
            ( pX p2 - pX p1
            , pY p2 - pY p1
            , pZ p2 - pZ p1
            )

-- | Given some points, generate lists containing all possible rotations of
-- the points.
rotateSpace :: [Position] -> [[Position]]
rotateSpace ps = L.nub . (ps :) . collate $ map allRotations ps
    where
        allRotations :: Position -> [Position]
        allRotations p =
            let xyRotates p_@Position{..} =
                    [ p_ {pX = pY, pY = negate pX}
                    , p_ {pX = negate pX, pY = negate pY}
                    , p_ {pX = negate pY, pY = pX}
                    ]
                xzRotates p_@Position{..} =
                    [ p_ {pX = pZ, pZ = negate pX}
                    , p_ {pX = negate pZ, pZ = pX}
                    , p_ {pX = negate pX, pZ = negate pZ}
                    ]
                yzRotates p_@Position{..} =
                    [ p_ {pY = pZ, pZ = negate pY}
                    , p_ {pY = negate pZ, pZ = pY}
                    , p_ {pY = negate pY, pZ = negate pZ}
                    ]
            in
            concat
                [ xyRotates p
                , xzRotates p
                , yzRotates p
                , concatMap xzRotates $ xyRotates p
                , concatMap xzRotates $ yzRotates p
                , concatMap yzRotates $ xyRotates p
                , concatMap yzRotates $ xzRotates p
                , concatMap xyRotates $ xzRotates p
                , concatMap xyRotates $ xzRotates p
                ]
        -- transform from:
        --      [ [ a, a ]
        --      , [ b, b ]
        --      ]
        -- to:
        --      [ [ a, b ]
        --      , [ a, b ]
        --      ]
        collate :: [[a]] -> [[a]]
        collate = \case
            ([] : _) -> []
            xs -> map head xs : collate (map tail xs)


-- PARSE

data Scanner =
    Scanner
        { sId :: !Int
        , sBeacons :: ![Position]
        , sRotations :: ![[(Position, [(Int, Int, Int)])]]
        } deriving (Show, Eq)

parseScanner :: ReadP Scanner
parseScanner = do
    void $ string "--- scanner "
    sId <- parseInt
    void $ string " ---" <* newline
    sBeacons <- sepBy1 parsePosition newline
    let sRotations = map positionVectorSet $ rotateSpace sBeacons
    void newline
    return Scanner {..}


data Position =
    Position
        { pX :: !Int
        , pY :: !Int
        , pZ :: !Int
        } deriving (Show, Eq, Ord)

parsePosition :: ReadP Position
parsePosition = do
    [pX, pY, pZ] <- sepBy1 parseInt $ char ','
    return Position{..}
