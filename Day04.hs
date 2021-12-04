{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Day04 where

import Control.Monad (void, foldM, replicateM)
import Data.Either (isRight, fromLeft)
import Data.Maybe (isJust)
import Data.List (foldl')
import Text.ParserCombinators.ReadP

import Harness
import ParseHelper


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
        , numberIndexes :: MyMap Int Int
        -- ^ Number -> Index
        , calledNumbers :: MySet Int
        } deriving (Show, Read)

mkBoard :: [Int] -> Board
mkBoard cellList =
    let numberIndexes = mkMap $ zip cellList [0 .. ]
        calledNumbers = setEmpty
    in  Board {..}

parseBoard :: ReadP Board
parseBoard = do
    let cellRow = replicateM 5 (many (char ' ') *> parseInt)
    cells <- replicateM 5 (cellRow <* newline)
    return $ mkBoard $ concat cells

markNewNumber :: Int -> Board -> Board
markNewNumber n b@Board{..} =
    b { calledNumbers = setInsert n calledNumbers }

getIndexOfNumber :: Int -> Board -> Maybe Int
getIndexOfNumber n Board{..} =
    mapLookup n numberIndexes

getUncalledNumbers :: Board -> [Int]
getUncalledNumbers Board{calledNumbers, numberIndexes} =
    filter (not . flip setContains calledNumbers) . map fst $ mapToList numberIndexes

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
            all (\i -> setContains (cellList !! i) calledNumbers) is

calculateScore :: Int -> [Int] -> Int
calculateScore lastCalled uncalled =
    sum uncalled * lastCalled



-- i wanna stay in base so bad lol
--
-- TODO: make these balanced
-- https://www.cmi.ac.in/~madhavan/courses/prog2-2012/lectures/balanced-search-trees-in-haskell.txt

data MyMap a b
    = Branch (MyMap a b) a b (MyMap a b)
    | Leaf
    deriving (Show, Read, Eq, Ord)

mkMap :: Ord a => [(a, b)] -> MyMap a b
mkMap = foldl' (\acc (k, v) -> mapInsert k v acc) mapEmpty

mapEmpty :: MyMap a b
mapEmpty = Leaf

mapInsert :: Ord a => a -> b -> MyMap a b -> MyMap a b
mapInsert k v = \case
    Leaf ->
        Branch Leaf k v Leaf
    Branch l bKey bVal r ->
        case compare k bKey of
            EQ -> Branch l bKey v r
            LT -> Branch (mapInsert k v l) bKey bVal r
            GT -> Branch l bKey bVal (mapInsert k v r)

mapLookup :: Ord a => a -> MyMap a b -> Maybe b
mapLookup k = \case
    Leaf -> Nothing
    Branch l bKey bVal r ->
        case compare k bKey of
            EQ -> Just bVal
            LT -> mapLookup k l
            GT -> mapLookup k r

mapToList :: MyMap a b -> [(a, b)]
mapToList = \case
    Leaf -> []
    Branch l k v r -> (k, v) : mapToList l <> mapToList r


newtype MySet a
    = MySet
        { fromMySet :: MyMap a ()
        } deriving (Show, Read, Eq)

setEmpty :: MySet a
setEmpty = MySet mapEmpty

setInsert :: Ord a => a -> MySet a -> MySet a
setInsert k = MySet . mapInsert k () . fromMySet

setContains :: Ord a => a -> MySet a -> Bool
setContains k = isJust . mapLookup k . fromMySet
