{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day21 where

import Control.Arrow    ((&&&))
import Control.Monad
import Data.Array       (Array)
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Map         (Map)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace


main :: IO ()
main = getInputAndSolve (parseInput parsePlayer) playPracticeGame playQuantumGameArray


-- SOLVE

playPracticeGame :: [Player] -> Integer
playPracticeGame initialPlayers =
    either id (error . ("unexpected result: " <>) . show) $ foldM playRound (0, 0, initialPlayers) [0 ..]
    where
        playRound :: (Integer, Integer, [Player]) -> Integer -> Either Integer (Integer, Integer, [Player])
        playRound (lastDiceRoll, totalRolls, ps) _ =
            let roundResult = foldM doTurn (lastDiceRoll, totalRolls, []) ps
            in  case roundResult of
                    Right x -> Right $ second reverse x
                    Left (rolls, winner) ->
                        let loser = fromJust $ L.find ((/= winner) . pId) ps
                        in  Left $ rolls * pScore loser
        doTurn :: (Integer, Integer, [Player]) -> Player -> Either (Integer, Integer) (Integer, Integer, [Player])
        doTurn (lastDiceRoll, totalRolls, finishedPlayers) p =
            let firstRoll = rollDie lastDiceRoll
                secondRoll = rollDie firstRoll
                thirdRoll = rollDie secondRoll
                totalMoves = firstRoll + secondRoll + thirdRoll
                player = applyMove totalMoves p
            in  if pScore player >= 1000 then
                    Left (totalRolls + 3, pId p)
                else
                    Right (thirdRoll, totalRolls + 3, player : finishedPlayers)
        rollDie :: Integer -> Integer
        rollDie = \case
            100 ->
                1
            d ->
                succ d

-- This one actually works! Although it seems like the same logic as the
-- DynProg version but uses an Array instead of a Map.
playQuantumGameArray :: [Player] -> Integer
playQuantumGameArray = \case
    [p1, p2] ->
        uncurry max $ winArray A.! (0, pPosition p1, pScore p1, pPosition p2, pScore p2)
    _ ->
        error "Expected 2 Players"
    where
        -- self-recursive array that:
        -- alternates turns
        -- sums wins for all possible moves when score is below 21
        -- marks a single win when a score is at/above 21
        winArray :: Array (Int, Integer, Integer, Integer, Integer) (Integer, Integer)
        winArray = A.listArray arrBounds
            [ if
                | p1Sc >= 21 -> (1, 0)
                | p2Sc >= 21 -> (0, 1)
                | otherwise  -> foldr
                    (\nextMove (p1Wins, p2Wins) ->
                        if turn == 0 then
                            let
                                p1Pos_ = ((p1Pos - 1 + nextMove) `mod` 10) + 1
                                p1Sc_  = p1Sc + p1Pos_
                                (p1W_, p2W_) = winArray A.! (1, p1Pos_, p1Sc_, p2Pos, p2Sc)
                            in
                                (p1Wins + p1W_, p2Wins + p2W_)
                        else
                            let
                                p2Pos_ = ((p2Pos - 1 + nextMove) `mod` 10) + 1
                                p2Sc_  = p2Sc + p2Pos_
                                (p1W_, p2W_) = winArray A.! (0, p1Pos, p1Sc, p2Pos_, p2Sc_)
                            in
                                (p1Wins + p1W_, p2Wins + p2W_)
                    )
                    (0, 0)
                    moves
            | let moves = sum <$> replicateM 3 [1, 2, 3]
            , (turn, p1Pos, p1Sc, p2Pos, p2Sc) <- A.range arrBounds
            ]
        -- Turn number, p1 position + score, p2 position + score
        arrBounds
            ::  ( (Int, Integer, Integer, Integer, Integer)
                , (Int, Integer, Integer, Integer, Integer)
                )
        arrBounds = ((0, 1, 0, 1, 0), (1, 10, 30, 10, 30))

-- this is fast, seem correct, & returns an answer of the right magnitude,
-- but the result is wrong.
playQuantumGameDynProg :: [Player] -> Integer
playQuantumGameDynProg = \case
    [p1, p2] ->
        uncurry max . traceShowId . snd $ getWins M.empty (p1, p2) True
    _ ->
        error "Expected 2 Players"
    where
        getWins
            :: Map (Player, Player) (Integer, Integer)
            -> (Player, Player)
            -> Bool
            -> (Map (Player, Player) (Integer, Integer), (Integer, Integer))
        getWins winCountMap ps@(player1, player2) p2MovedLast =
            case M.lookup ps winCountMap of
                Just (l, r) ->
                    (winCountMap, (l, r))
                Nothing
                    | pScore player1 >= 21 ->
                        (M.insert ps (1, 0) winCountMap, (1, 0))
                    | pScore player2 >= 21 ->
                        (M.insert ps (0, 1) winCountMap, (0, 1))
                    | otherwise ->
                        let
                            possibleMoves :: [(Integer, Integer)]
                            possibleMoves =
                                map (toInteger . length &&& head)
                                    . L.group
                                    $ L.sort [sum m | m <- replicateM 3 [1, 2, 3]]
                            uniquePossibleStates :: [(Integer, (Player, Player))]
                            uniquePossibleStates =
                                    [ (mCount,) $ if p2MovedLast then
                                        (applyMove moves player1, player2)
                                      else
                                        (player1, applyMove moves player2)
                                    | (mCount, moves) <- possibleMoves
                                    ]
                        in
                            (\(m, ws) -> (M.insert ps ws m, ws)) $ L.foldl'
                                (\(winMap, (p1W, p2W)) (stateCount, state) ->
                                    let (!m, (!p1, !p2)) =
                                            getWins winMap state (not p2MovedLast)
                                    in  ( m
                                        , ( p1 * stateCount + p1W
                                          , p2 * stateCount + p2W
                                          )
                                        )
                                )
                                (winCountMap, (0, 0))
                                uniquePossibleStates



-- This uses list monad, which takes forever to run & lotsa RAM
playQuantumGame :: [Player] -> Int
playQuantumGame ps =
    let (length -> p1Wins, length -> p2Wins) = L.partition (== 1) games
    in
        max p1Wins p2Wins
    where
        games :: [Integer]
        games =
            playUntilWon [] ps
        playUntilWon :: [Player] -> [Player] -> [Integer]
        playUntilWon havePlayed = \case
            [] -> playUntilWon [] $ reverse havePlayed
            nextPlayer : otherPlayers -> do
                rollTotal <- sum <$> replicateM 3 [1, 2, 3]
                let scored = increaseScore $ move rollTotal nextPlayer
                if pScore scored >= 21 then
                    return $ pId scored
                else
                    playUntilWon (scored : havePlayed) otherPlayers


-- HELPERS

applyMove :: Integer -> Player -> Player
applyMove spaces =
    increaseScore . move spaces

move :: Integer -> Player -> Player
move moves p =
    p { pPosition = ((pPosition p - 1 + moves) `mod` 10) + 1 }

increaseScore :: Player -> Player
increaseScore p =
    p { pScore = pScore p + pPosition p }


-- PARSE

data Player =
    Player
        { pId :: !Integer
        , pPosition :: !Integer
        , pScore :: !Integer
        } deriving (Show, Eq)

instance Ord Player where
    compare = compare `on` \Player{..} -> (pPosition, pScore)

parsePlayer :: ReadP Player
parsePlayer = do
    void $ string "Player "
    pId <- toInteger <$> parseInt
    void $ string " starting position: "
    pPosition <- toInteger <$> parseInt
    let pScore = 0
    return Player{..}
