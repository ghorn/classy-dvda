{-# OPTIONS_GHC -Wall #-}

module Classy.Utils ( lastCommonBases
                    , removeCommonList
                    ) where

import Classy.Types

-- | Return the direct dependency of a frame
parentBases :: Bases -> Bases
parentBases NewtonianBases = NewtonianBases
parentBases (RotatedBases frame0 _ _) = frame0

-- | Return the lineage of frames, the last entry always being the newtonian frame
parentBasesChain :: Bases -> [Bases]
parentBasesChain NewtonianBases = [NewtonianBases]
parentBasesChain frame = frame:(parentBasesChain $ parentBases frame)

-- | Given two frames, finds the closest ancestor frame (in terms of dependency)
-- If all else fails, the newtonian reference frame is always a valid answer
lastCommonBases :: Bases -> Bases -> Bases
lastCommonBases frameA frameB = lastCommonList (reverse $ parentBasesChain frameA) (reverse $ parentBasesChain frameB)  NewtonianBases

-- | Given two lists, traverses both lists as long as their entries are equal and returs the last equal entry
-- Example:   lastCommonList [0,4,5] [0] 42  =>  0
-- Example:   lastCommonList [0,4,5] [4] 42  =>  42  (nothing was common, outputing default )
-- Example:   lastCommonList [0,4,5] [0,4,12] 42  =>  4 
-- Example:   lastCommonList [0,4,5] [0,4,5] 42  =>  5  
lastCommonList :: Eq a => [a] -> [a] -> a -> a 
lastCommonList [] _ n = n
lastCommonList _ [] n = n
lastCommonList (f1:t1) (f2:t2) last'
  | f1 == f2  = lastCommonList t1 t2 f1
  | otherwise = last'

-- | Given two lists, keeps chopping of the heads of these lists while entries compare to equal
-- removeCommonList [4,5] [1,2,3]  => ([4,5],[1,2,3])
-- removeCommonList [1,4] [1,2,3]  => ([5],[2,3])
-- removeCommonList [1,2] [1,2,3]  => ([],[3])
removeCommonList :: Eq a => [a] -> [a] -> ([a],[a])
removeCommonList [] other  = ([], other)
removeCommonList other  [] = (other, [])
removeCommonList (f1:t1) (f2:t2)
  | f1 == f2  = removeCommonList t1 t2
  | otherwise = (f1:t1, f2:t2)
