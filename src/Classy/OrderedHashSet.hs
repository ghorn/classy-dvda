{-# OPTIONS_GHC -Wall #-}

-- this whole module simply defines a data type OrderedHashSet which emulates HashSet except that
-- it has an additional function toOrderedList where the returned list is ordered by insersion order
module Classy.OrderedHashSet ( OrderedHashSet
                             , insert
                             , member
                             , empty
                             , toList
                             , toOrderedList
                             ) where

import Data.Hashable
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS

data OrderedHashSet a = OrderedHashSet (HashSet a) [a] deriving Show

member :: (Eq a, Hashable a) => a -> OrderedHashSet a -> Bool
member x (OrderedHashSet hs _) = HS.member x hs

insert :: (Eq a, Hashable a) => a -> OrderedHashSet a -> OrderedHashSet a
insert x ohs
  | x `member` ohs = ohs
insert x (OrderedHashSet hs xs) = OrderedHashSet (HS.insert x hs) (xs ++ [x])

empty :: OrderedHashSet a
empty = OrderedHashSet HS.empty []

toList :: OrderedHashSet a -> [a]
toList (OrderedHashSet hs _) = HS.toList hs

toOrderedList :: OrderedHashSet t -> [t]
toOrderedList (OrderedHashSet _ xs) = xs
