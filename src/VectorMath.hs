{-# OPTIONS_GHC -Wall #-}

module VectorMath ( ddt
                  , ddtN
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
                  , angVelWrtN
                  , angVelWrtF
                  , time
                  , xyzVec
                  ) where

import Data.Maybe ( catMaybes )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Debug.Trace
import Dvda
import Dvda.Expr ( Expr(..), Sym(..) )

import Types

-- | express a vector as x/y/z components of a given frame
xyzVec :: (Sca,Sca,Sca) -> Frame -> Vec
xyzVec (sx,sy,sz) frame =
  scaleBasis sx (Basis frame X) +
  scaleBasis sy (Basis frame Y) +
  scaleBasis sz (Basis frame Z)

ddt :: Sca -> Sca
ddt = flip partial (SExpr time Nothing)


-- | time derivative in a rotating frame w.r.t the newtonian frame
ddtN :: Vec -> Vec
ddtN vec = ddtF vec NewtonianFrame 

-- | time derivative in a rotating frame w.r.t a given frame
ddtF :: Vec -> Frame -> Vec
ddtF (Vec hm0) baseFrame = removeZeros $ (\(x,y) -> sum x + sum y) $ unzip $ map ddtF' (HM.toList hm0)
  where
    ddtF' :: (Basis, Sca) -> (Vec,Vec)
    ddtF' (basis, sca) = (scaleBasis (ddt sca) basis, ddtFBasis basis) -- add these up at the end (improves symbolic simplification)
      where
        ddtFBasis :: Basis -> Vec
        ddtFBasis (Basis bf _) = (angVelWrtF bf baseFrame) `cross` scaleBasis sca basis
        ddtFBasis (Cross bf0 bf1) = (ddtF v0 baseFrame) `cross` v1 + v0 `cross` (ddtF v1 baseFrame)
          where
            v0 = scaleBasis 1 bf0
            v1 = scaleBasis 1 bf1

-- | Given two frames, finds the closest ancestor frame (in terms of dependency)
latestCommonFrame :: Frame -> Frame -> Frame
-- | A rather silly implementation
latestCommonFrame _ _ = NewtonianFrame


-- | Angular velocity of a frame w.r.t the Newtonian frame
angVelWrtN :: Frame -> Vec
angVelWrtN frame = angVelWrtF frame NewtonianFrame 

--------------------------------------------------------------------
-- | Angular velocity of a frame w.r.t a second frame
angVelWrtF :: Frame -> Frame -> Vec
angVelWrtF frameA frameB = (angVelWrtF' frameA common) - (angVelWrtF' frameB common)
  where
    common = latestCommonFrame frameA frameB
    angVelWrtF' :: Frame -> Frame -> Vec
    angVelWrtF' frameA_ frameB_
      | frameA_ == frameB_ = zeroVec
    angVelWrtF' (RotatedFrame frame0 (RotCoord q) _) baseFrame = angVelWrtF' frame0 baseFrame + partialV q (SExpr time Nothing)
    angVelWrtF' b@(RotatedFrame frame0 (RotSpeed (wx,wy,wz)) _) baseFrame = angVelWrtF' frame0 baseFrame + w
      where
        w = xyzVec (wx,wy,wz) b
    angVelWrtF' NewtonianFrame anything = trace "angVelWrtF: THIS SHOULD NEVER HAPPEN" $ negate (angVelWrtN anything)


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

    allReplacements :: Basis -> Frame -> [Basis]
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

    allReplacements :: Basis -> Frame -> [Basis]
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

-- | dyad dot vector - x*b1>b2> `dyadDot` y*b3> = (x*y*(b2> . b3>)) * b1>
dyadDot :: Dyad -> Vec -> Vec
dyadDot (Dyad x b1 b2) vec= scaleBasis scalar b1
  where
    scalar = scaleBasis x b2 `dot` vec

dyadicDot :: Dyadic -> Vec -> Vec
dyadicDot (Dyadic ((xx,xy,xz), (yx,yy,yz), (zx,zy,zz))) vec = sum $ map (`dyadDot` vec) [xx,xy,xz,yx,yy,yz,zx,zy,zz]

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
