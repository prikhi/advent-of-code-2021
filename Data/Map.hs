{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Map
    ( Map
    , empty
    , fromList
    , toList
    , insert
    , lookup
    )
    where

import Data.List (foldl')

import Prelude hiding (lookup)


-- TODO: balance on construction, re-balance on modification
-- https://www.cmi.ac.in/~madhavan/courses/prog2-2012/lectures/balanced-search-trees-in-haskell.txt

data Map a b
    = Branch !(BranchData a b)
    | Leaf
    deriving (Show, Read, Eq, Ord)

data BranchData a b =
    BranchData
        { bdLeft :: !(Map a b)
        , bdKey :: !a
        , bdVal :: !b
        , bdRight :: !(Map a b)
        } deriving (Show, Read, Eq, Ord)


empty :: Map a b
empty = Leaf

fromList :: Ord a => [(a, b)] -> Map a b
fromList = foldl' (\acc (k, v) -> insert k v acc) empty

toList :: Map a b -> [(a, b)]
toList = \case
    Leaf -> []
    Branch BranchData {..} -> concat [toList bdLeft, [(bdKey, bdVal)], toList bdRight]

insert :: Ord a => a -> b -> Map a b -> Map a b
insert !k !v = \case
    Leaf ->
        Branch $ BranchData Leaf k v Leaf
    Branch bd@BranchData{..} ->
        case compare k bdKey of
            EQ -> Branch bd { bdVal = v }
            LT -> Branch bd { bdLeft = insert k v bdLeft }
            GT -> Branch bd { bdRight = insert k v bdRight }

lookup :: Ord a => a -> Map a b -> Maybe b
lookup k = \case
    Leaf -> Nothing
    Branch BranchData{..} ->
        case compare k bdKey of
            EQ -> Just bdVal
            LT -> lookup k bdLeft
            GT -> lookup k bdRight
