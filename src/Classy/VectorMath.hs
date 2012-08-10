{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}

module Classy.VectorMath ( cross
                         , dot
                         , dyadDot
                         , dyadicDot
                         , scale
                         , scaleBasis
                         , xyzVec
                         , subtractPoints
                         ) where

import Data.Maybe ( catMaybes )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Classy.Types
import Classy.Utils ( removeCommonList )

-- | express a vector as x/y/z components of a given frame
xyzVec :: (Sca,Sca,Sca) -> Bases -> Vec
xyzVec (sx,sy,sz) frame =
  scaleBasis sx (Basis frame X) +
  scaleBasis sy (Basis frame Y) +
  scaleBasis sz (Basis frame Z)

-- | if (a x b) is zero, return Nothing
--   .
--   if (a x b) is non-zero, return (basis0 x basis1, sign*scalar0*scalar1)
crossBases :: (Basis, Sca) -> (Basis, Sca) -> Maybe (Basis, Sca)
crossBases (b0@(Basis f0 xyz0), s0) (b1@(Basis f1 xyz1), s1)
  | f0 == f1 = case (xyz0, xyz1) of
    (X,Y) -> Just (Basis f0 Z, s0*s1)
    (Y,Z) -> Just (Basis f0 X, s0*s1)
    (Z,X) -> Just (Basis f0 Y, s0*s1)
    (Z,Y) -> Just (Basis f0 X, -(s0*s1))
    (Y,X) -> Just (Basis f0 Z, -(s0*s1))
    (X,Z) -> Just (Basis f0 Y, -(s0*s1))
    (X,X) -> Nothing
    (Y,Y) -> Nothing
    (Z,Z) -> Nothing
  | otherwise = case (b0Equivs, b1Equivs) of
    (b0':_,     _) -> crossBases (b0',s0) (b1 ,s1)
    (    _, b1':_) -> crossBases (b0 ,s0) (b1',s1)
    ([],[]) -> Just (Cross b0 b1, s0*s1)
  where
    -- every basis b0 is equivalent to
    b0Equivs :: [Basis]
    b0Equivs = allReplacements b0 f1
    -- every basis b1 is equivalent to
    b1Equivs :: [Basis]
    b1Equivs = allReplacements b1 f0

    allReplacements :: Basis -> Bases -> [Basis]
    allReplacements b f = map snd $ filter g ebs
      where
        g (b',Basis f' _) = b == b' && f == f'
        g _ = False

    -- all the (basis1, basis2) equivilencies
    ebs :: [(Basis,Basis)]
    ebs = ebs' ++ map (\(x,y) -> (y,x)) ebs'
      where
        ebs' = HS.toList $ HS.union (equivBases b0) (equivBases b1)
crossBases (b0,s0) (b1,s1) = Just (Cross b0 b1, s0*s1)

-- | vector cross product
cross :: Vec -> Vec -> Vec
cross (Vec hm0) (Vec hm1) = removeZeros $ Vec hm
  where
    hm = HM.fromListWith (+) $
         catMaybes [crossBases (b0,s0) (b1,s1) | (b0,s0) <- HM.toList hm0, (b1,s1) <- HM.toList hm1]

---- | dot product of two vectors
--   if (a . b) is zero, return Nothing
--   .
--   if (a . b) is non-zero, return (basis0 . basis1, sign*scalar0*scalar1)
dotBases :: (Basis, Sca) -> (Basis, Sca) -> Sca
dotBases (b0@(Basis f0 xyz0), s0) (b1@(Basis f1 xyz1), s1)
  | f0 == f1 = case (xyz0, xyz1) of
    (X,X) -> s0*s1
    (Y,Y) -> s0*s1
    (Z,Z) -> s0*s1
    _     -> 0
  | otherwise = case (b0Equivs, b1Equivs) of
    (b0':_,     _) -> dotBases (b0',s0) (b1 ,s1)
    (    _, b1':_) -> dotBases (b0 ,s0) (b1',s1)
    ([],[]) -> SDot (b0,b1) (s0*s1)
  where
    -- every basis b0 is equivalent to
    b0Equivs :: [Basis]
    b0Equivs = allReplacements b0 f1
    -- every basis b1 is equivalent to
    b1Equivs :: [Basis]
    b1Equivs = allReplacements b1 f0

    allReplacements :: Basis -> Bases -> [Basis]
    allReplacements b f = map snd $ filter g ebs
      where
        g (b',Basis f' _) = b == b' && f == f'
        g _ = False

    -- all the (basis1, basis2) equivilencies
    ebs :: [(Basis,Basis)]
    ebs = ebs' ++ map (\(x,y) -> (y,x)) ebs'
      where
        ebs' = HS.toList $ HS.union (equivBases b0) (equivBases b1)
dotBases (b0,s0) (b1,s1) = SDot (b0,b1) (s0*s1)

-- | vector dot product
dot :: Vec -> Vec -> Sca
dot (Vec hm0) (Vec hm1) =
  sum [dotBases (b0,s0) (b1,s1) | (b0,s0) <- HM.toList hm0, (b1,s1) <- HM.toList hm1]

-- | dyad dot vector: x*b1>b2> `dyadDot` y*b3>     = (x*y*(b2> . b3>)) * b1>
--   vector dot dyad: x*b1>    `dyadDot` y*b2>b3>  = (x*y*(b1> . b2>)) * b3>
class DyadDot a b where
  dyadDot :: a -> b -> Vec
instance DyadDot Dyad Vec where
  dyadDot (Dyad x b1 b2) vec = scaleBasis scalar b1
    where
      scalar = scaleBasis x b2 `dot` vec
instance DyadDot Vec Dyad where
  dyadDot vec (Dyad x b1 b2) = scaleBasis scalar b2
    where
      scalar = vec `dot` scaleBasis x b1

class DyadicDot a b where
  dyadicDot :: a -> b -> Vec
instance DyadicDot Dyadic Vec where
  dyadicDot (Dyadic ((xx,xy,xz), (yx,yy,yz), (zx,zy,zz))) vec =
    sum $ map (`dyadDot` vec) [xx,xy,xz,yx,yy,yz,zx,zy,zz]
instance DyadicDot Vec Dyadic where
  dyadicDot vec (Dyadic ((xx,xy,xz), (yx,yy,yz), (zx,zy,zz))) =
    sum $ map (vec `dyadDot`) [xx,xy,xz,yx,yy,yz,zx,zy,zz]

-- | scale a vector by a scalar, returning a vector
scale :: Sca -> Vec -> Vec
scale s vec@(Vec hm)
  | isVal 0 s = zeroVec
  | isVal 1 s = vec
  | isVal (-1) s = Vec (HM.map negate hm)
  | otherwise = removeZeros $ Vec (HM.map (s *) hm)

-- | combine a scalar and a basis into a vector
scaleBasis :: Sca -> Basis -> Vec
scaleBasis s b = removeZeros $ Vec (HM.singleton b s)

-- | subtractPoints x y = x - y
subtractPoints :: Point -> Point -> Vec
subtractPoints vx vy = sum a - sum b
  where 
    (a,b) = removeCommonList (reverse $ vecsFromN0 vx) (reverse $ vecsFromN0 vy)
