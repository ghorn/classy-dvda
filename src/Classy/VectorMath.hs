{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}

module Classy.VectorMath ( ddt
                         , ddtN
                         , ddtNp
                         , ddtF
                         , partial
                         , partialV
                         , cross
                         , dot
                         , dyadDot
                         , dyadicDot
                         , scale
                         , scaleBasis
                         , isCoord
                         , isSpeed
                         , angVelWrt
                         , angVelWrtN
                         , time
                         , xyzVec
                         , parentBases
                         , subtractPoints
                         ) where

import Data.Maybe ( catMaybes )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Debug.Trace
import Dvda
import Dvda.Expr ( Expr(..), Sym(..) )

import Classy.Types

-- | express a vector as x/y/z components of a given frame
xyzVec :: (Sca,Sca,Sca) -> Bases -> Vec
xyzVec (sx,sy,sz) frame =
  scaleBasis sx (Basis frame X) +
  scaleBasis sy (Basis frame Y) +
  scaleBasis sz (Basis frame Z)

ddt :: Sca -> Sca
ddt = flip partial (SExpr time Nothing)

-- | time derivative in a rotating frame w.r.t the newtonian frame
ddtN :: Vec -> Vec
ddtN vec = ddtF vec NewtonianBases 

-- | time derivative in a rotating frame w.r.t a given frame
ddtF :: Vec -> Bases -> Vec
ddtF (Vec hm0) baseFrame = removeZeros $ (\(x,y) -> sum x + sum y) $ unzip $ map ddtF' (HM.toList hm0)
  where
    ddtF' :: (Basis, Sca) -> (Vec,Vec)
    ddtF' (basis, sca) = (scaleBasis (ddt sca) basis, ddtFBasis basis) -- add these up at the end (improves symbolic simplification)
      where
        ddtFBasis :: Basis -> Vec
        ddtFBasis (Basis bf _) = (angVelWrt bf baseFrame) `cross` scaleBasis sca basis
        ddtFBasis (Cross bf0 bf1) = (ddtF v0 baseFrame) `cross` v1 + v0 `cross` (ddtF v1 baseFrame)
          where
            v0 = scaleBasis 1 bf0
            v1 = scaleBasis 1 bf1

-- | time derivative in a rotating frame using golden rule of vector differentiation
ddtNp :: Point -> Vec
ddtNp N0 = 0
ddtNp (RelativePoint p0 vec) = ddtN vec + ddtNp p0

--------------------------------------------------------------------

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

-- | Angular velocity of a frame w.r.t the Newtonian frame
angVelWrtN :: Bases -> Vec
angVelWrtN frame = angVelWrt frame NewtonianBases 

--------------------------------------------------------------------
-- | angVelWrt f g is the angular velocity of frame f with respect to frame g
angVelWrt :: Bases -> Bases -> Vec
angVelWrt frameA frameB = (angVelWrtCommon frameA) - (angVelWrtCommon frameB)
  where
    common = lastCommonBases frameA frameB

    angVelWrtCommon :: Bases -> Vec
    angVelWrtCommon fr
      | fr == common = 0
    angVelWrtCommon (RotatedBases frame0 (RotCoord q) _) = angVelWrtCommon frame0 + partialV q (SExpr time Nothing)
    angVelWrtCommon b@(RotatedBases frame0 (RotSpeed (wx,wy,wz)) _) = angVelWrtCommon frame0 + w
      where
        w = xyzVec (wx,wy,wz) b
    angVelWrtCommon NewtonianBases = trace "angVelWrt: THIS SHOULD NEVER HAPPEN" $ negate (angVelWrtN common)


isCoord, isSpeed, isTime :: Sca -> Bool
isCoord (SExpr (ESym (SymDependent _ 0 (Sym t))) (Just 0)) = ESym (Sym t) == time
isCoord _ = False

isSpeed (SExpr (ESym (SymDependent _ _ (Sym t))) (Just 1)) = ESym (Sym t) == time
isSpeed _ = False

isTime (SExpr t Nothing) = t == time
isTime _ = False

-- | partial derivative, if the argument is time this will be the full derivative
partial :: Sca -> Sca -> Sca
partial _ (SMul _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SDiv _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SAdd _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SSub _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SNeg _)   = error "partial taken w.r.t. non-symbolic"
partial _ (SDot _ _) = error "partial taken w.r.t. non-symbolic"
partial (SNeg x) arg = -(partial x arg)
partial (SMul x y) arg = x*y' + x'*y
  where
    x' = partial x arg
    y' = partial y arg
partial (SDiv x y) arg = x'/y - x/(y*y)*y'
  where
    x' = partial x arg
    y' = partial y arg
partial (SAdd x y) arg = partial x arg + partial y arg
partial (SSub x y) arg = partial x arg - partial y arg
partial (SExpr x (Just k)) a@(SExpr arg _)
  | isTime a  = SExpr (head (rad x [arg])) (Just (k+1))
  | otherwise = SExpr (head (rad x [arg])) (Just k)
partial (SExpr x Nothing) (SExpr arg _) = SExpr (head (rad x [arg])) Nothing
partial (SDot (b0,b1) s) arg = SDot (b0,b1) 1 * partial s arg + dDot*s
  where
    v0 = scaleBasis 1 b0
    v1 = scaleBasis 1 b1
    dDot
      | isTime arg = ddtN v0 `dot` v1 + v0 `dot` ddtN v1
      | isSpeed arg = 0
      | otherwise = error "can't take partial of SDot w.r.t anything except time or generalied speed"

-- | partial derivative, if the argument is time this will be a full derivative
--   but will not apply the golden rule of vector differentiation
partialV :: Vec -> Sca -> Vec
partialV (Vec hm) arg = removeZeros $ Vec (HM.map (`partial` arg) hm)


------------------------------ utilities -------------------------------------

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

-- | the independent variable time used in taking time derivatives
time :: Expr Double
time = sym "t"

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

-- | subtractPoints x y = x - y
subtractPoints :: Point -> Point -> Vec
subtractPoints vx vy = sum a - sum b
  where 
    (a,b) = removeCommonList (reverse $ vecsFromN0 vx) (reverse $ vecsFromN0 vy)
