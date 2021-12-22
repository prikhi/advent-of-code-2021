{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day21 where

import Control.Arrow ((&&&))
import Control.Monad
import Data.Bifunctor
import Data.Function
import Data.Maybe
import Data.Map         (Map)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace


main :: IO ()
main = getInputAndSolve (parseInput parsePlayer) playPracticeGame playQuantumGameDynProg


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

-- TODO: this is reasonably fast, seem correct, & returns an answer of the
-- right magnitude, but the result is wrong.
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
