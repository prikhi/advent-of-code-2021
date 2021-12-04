{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day04 where

import Control.Monad (void, foldM, replicateM)
import Data.Either (isRight, fromLeft)
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper

import qualified Data.Map as M
import qualified Data.Set as S


main :: IO ()
main = do
    getInputAndSolve parse (uncurry findWinner) (uncurry findLoser)
    where
        parse :: String -> ([Int], [Board])
        parse = parseInputRaw $ do
            moves <- parseIntArray Nothing Nothing
            void $ newline *> newline
            bs <- sepBy parseBoard newline
            return (moves, bs)


findWinner :: [Int] -> [Board] -> Int
findWinner ns bs =
    fromLeft (error "tie") $ foldM (\bs_ n -> mapM (callNumber n) bs_) bs ns

findLoser :: [Int] -> [Board] -> Int
findLoser nums bs_ =
    either id (error "no boards") $ foldM go bs_ nums
    where
        go :: [Board] -> Int -> Either Int [Board]
        go bs n = case bs of
            [b] -> (: []) <$> callNumber n b
            [] -> error "no boards"
            _ ->
                sequence . filter isRight $ map (callNumber n) bs


data Board =
    Board
        { cellList :: [Int]
        -- ^ Numbers
        , numberIndexes :: M.Map Int Int
        -- ^ Number -> Index
        , calledNumbers :: S.Set Int
        } deriving (Show, Read)

mkBoard :: [Int] -> Board
mkBoard cellList =
    let numberIndexes = M.fromList $ zip cellList [0 .. ]
        calledNumbers = S.empty
    in  Board {..}

parseBoard :: ReadP Board
parseBoard = do
    let cellRow = replicateM 5 (many (char ' ') *> parseInt)
    cells <- replicateM 5 (cellRow <* newline)
    return $ mkBoard $ concat cells

markNewNumber :: Int -> Board -> Board
markNewNumber n b@Board{..} =
    b { calledNumbers = S.insert n calledNumbers }

getIndexOfNumber :: Int -> Board -> Maybe Int
getIndexOfNumber n Board{..} =
    M.lookup n numberIndexes

getUncalledNumbers :: Board -> [Int]
getUncalledNumbers Board{calledNumbers, numberIndexes} =
    filter (not . flip S.contains calledNumbers) . map fst $ M.toList numberIndexes

callNumber :: Int -> Board -> Either Int Board
callNumber n (markNewNumber n -> b@Board{..}) =
    case getIndexOfNumber n b of
        Nothing -> return b
        Just i ->
            let
                (row, col) = i `quotRem` (5 :: Int)
                rowIndexes = map (\c -> row * 5 + c) [0 .. 4]
                colIndexes = map (\r -> r * 5 + col) [0 .. 4]
                rowWinner = boardWins rowIndexes
                colWinner = boardWins colIndexes
                uncalledNumbers = getUncalledNumbers b
            in
                if rowWinner || colWinner then
                    Left $ calculateScore n uncalledNumbers
                else
                    Right b
    where
        boardWins :: [Int] -> Bool
        boardWins is =
            all (\i -> S.contains (cellList !! i) calledNumbers) is

calculateScore :: Int -> [Int] -> Int
calculateScore lastCalled uncalled =
    sum uncalled * lastCalled
