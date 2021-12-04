module Data.Set
    ( Set
    , empty
    , contains
    , insert
    ) where

import Data.Maybe (isJust)

import qualified Data.Map as M


newtype Set a
    = Set
        { fromSet :: M.Map a ()
        } deriving (Show, Read, Eq)

empty :: Set a
empty = Set M.empty

insert :: Ord a => a -> Set a -> Set a
insert k = Set . M.insert k () . fromSet

contains :: Ord a => a -> Set a -> Bool
contains k = isJust . M.lookup k . fromSet
