{-# LANGUAGE LambdaCase #-}
module Day02 where

import Data.List (foldl')
import Control.Monad (void)
import Text.ParserCombinators.ReadP

import ParseHelper

main :: IO ()
main = do
    commands <- parseInput parseCommand <$> getContents
    putStrLn $ "Part 1: " <> show (makeMoves commands)
    putStrLn $ "Part 2: " <> show (makeAimedMoves commands)


data Command
    = Forward Int
    | Down Int
    | Up Int
    deriving (Show, Read, Eq)

parseCommand :: ReadP Command
parseCommand =
    choice
        [ intCmd "forward" Forward
        , intCmd "down" Down
        , intCmd "up" Up
        ]
    where
        intCmd :: String -> (Int -> Command) -> ReadP Command
        intCmd label tag = do
            void $ string label
            skipSpaces
            tag <$> parseInt

makeMoves :: [Command] -> Int
makeMoves = uncurry (*) . foldl' go (0, 0)
    where
        go :: (Int, Int) -> Command -> (Int, Int)
        go (horiz, depth) = \case
            Forward x -> (horiz + x, depth)
            Down x -> (horiz, depth + x)
            Up x -> (horiz, depth - x)

makeAimedMoves :: [Command] -> Int
makeAimedMoves = (\(x, y, _) -> x * y) . foldl' makeMove (0, 0, 0)
    where
        makeMove :: (Int, Int, Int) -> Command -> (Int, Int, Int)
        makeMove (horiz, depth, aim) = \case
            Down x -> (horiz, depth, aim + x)
            Up x -> (horiz, depth, aim - x)
            Forward x -> (horiz + x, depth + aim * x, aim)
