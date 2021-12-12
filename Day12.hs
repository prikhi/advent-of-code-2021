{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Day12 where

import Control.Arrow ((&&&))
import Data.Char
import Data.Map     (Map)
import Data.Maybe
import Data.Set     (Set)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S


main :: IO ()
main = getInputAndSolve (parseInputRaw parseGraph) (countPaths False) (countPaths True)


-- SOLVE

countPaths :: Bool -> Graph -> Int
countPaths canVisitTwice g =
    let start = fromJust $ M.lookup "start" $ graph g
    in  length $ pathsToEnd g S.empty (not canVisitTwice) start


-- HELPERS

pathsToEnd :: Graph -> Set String -> Bool -> Node -> [[String]]
pathsToEnd g@Graph{..} seenSmalls visitedSmallTwice currentNode =
    let
        newSeen =
            if isSmallCave currentNode then
                S.insert (label currentNode) seenSmalls
            else
                seenSmalls

        unseenSmallAdjacents =
            mapMaybe (`M.lookup` graph)
                . filter
                    (\n ->
                        isSmallCaveLabel n
                            && n /= "start"
                            && not (S.contains n newSeen && visitedSmallTwice)
                    )
                $ adjacent currentNode
        bigAdjacents =
            mapMaybe (`M.lookup` graph)
                . filter (not . isSmallCaveLabel)
                $ adjacent currentNode

        bigPaths =
            concatMap (pathsToEnd g newSeen visitedSmallTwice) bigAdjacents
        smallPaths =
            concatMap
                (\n ->
                    pathsToEnd
                        g
                        (S.insert (label n) newSeen)
                        (visitedSmallTwice || S.contains (label n) newSeen)
                        n
                ) unseenSmallAdjacents
    in
        if label currentNode == "end" then
            [["end"]]
        else
            map (label currentNode :) $ bigPaths <> smallPaths

isSmallCave :: Node -> Bool
isSmallCave = isSmallCaveLabel . label

isSmallCaveLabel :: String -> Bool
isSmallCaveLabel = isLower . head


-- PARSE

newtype Graph =
    Graph { graph :: Map String Node } deriving (Show, Read, Eq, Ord)

data Node =
    Node
        { label :: !String
        , adjacent :: ![String]
        } deriving (Show, Read, Eq, Ord)

parseGraph :: ReadP Graph
parseGraph = do
    edges <- sepBy parseEdge newline <* newline <* eof
    let nodes = M.fromList . map (label &&& id) . L.nub $ concatMap mkNodes edges
        withAdjacencies = foldr addAdjacencies nodes edges
    return $ Graph withAdjacencies
    where
        parseEdge :: ReadP (String, String)
        parseEdge =
            (,)
                <$> manyTill (satisfy isAlpha) (char '-')
                <*> many1 (satisfy isAlpha)
        mkNodes :: (String, String) -> [Node]
        mkNodes (from, to) =
            [ Node from []
            , Node to []
            ]
        addAdjacencies :: (String, String) -> M.Map String Node -> M.Map String Node
        addAdjacencies (from, to) m =
            let
                mbNewMap = do
                    fromNode <- M.lookup from m
                    toNode <- M.lookup to m
                    let newFrom = fromNode { adjacent = to : adjacent fromNode }
                        newTo = toNode { adjacent = from : adjacent toNode }
                    return $ M.insert from newFrom $ M.insert to newTo m
            in
                fromMaybe m mbNewMap
