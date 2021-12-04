{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
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
    = Branch !(Map a b) !a !b !(Map a b)
    | Leaf
    deriving (Show, Read, Eq, Ord)

empty :: Map a b
empty = Leaf

fromList :: Ord a => [(a, b)] -> Map a b
fromList = foldl' (\acc (k, v) -> insert k v acc) empty

toList :: Map a b -> [(a, b)]
toList = \case
    Leaf -> []
    Branch l k v r -> concat [toList l, [(k, v)], toList r]

insert :: Ord a => a -> b -> Map a b -> Map a b
insert !k !v = \case
    Leaf ->
        Branch Leaf k v Leaf
    Branch l bKey bVal r ->
        case compare k bKey of
            EQ -> Branch l bKey v r
            LT -> Branch (insert k v l) bKey bVal r
            GT -> Branch l bKey bVal (insert k v r)

lookup :: Ord a => a -> Map a b -> Maybe b
lookup k = \case
    Leaf -> Nothing
    Branch l bKey bVal r ->
        case compare k bKey of
            EQ -> Just bVal
            LT -> lookup k l
            GT -> lookup k r
