{-# OPTIONS_GHC -Wall #-}

module Frames ( ddt
              , ddtN
              , partial
              , partialV
              , cross
              , dot
              , scale
              , scaleBasis
              , time
              ) where

import Data.Maybe ( catMaybes )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS

import Dvda hiding ( scale, vec, Z )
import qualified Dvda as Dvda

import Types

ddt :: Sca -> Sca
ddt (SExpr x)
  | isVal 0 ret = SZero
  | isVal 1 ret = SOne
  | otherwise = ret
  where
    ret = SExpr $ head $ runDeriv x [time]
ddt _ = SZero

-- | time derivative in a rotating frame using golden rule of vector differentiation
ddtN :: Vec -> Vec
ddtN (Vec hm0) = removeZeros $ sum $ map ddtN' (HM.toList hm0)
  where
    ddtN' :: (Basis, Sca) -> Vec
    ddtN' (basis, sca) = scaleBasis (ddt sca) basis + ddtNBasis basis
      where
        ddtNBasis :: Basis -> Vec
        ddtNBasis (Basis bf _) = (angVelWrtN bf) `cross` (scaleBasis sca basis)
        ddtNBasis (Cross bf0 bf1) = ddtN v0 `cross` v1 + v0 `cross` ddtN v1
          where
            v0 = scaleBasis 1 bf0
            v1 = scaleBasis 1 bf1

--------------------------------------------------------------------
angVelWrtN :: Frame -> Vec
angVelWrtN (NewtonianFrame _) = zeroVec
--angVelWrtN (RFrame frame0 (RotCoordSpeed _ w) _ _) = (angVelWrtN frame0) + w
angVelWrtN (RFrame frame0 (RotSpeed w) _) = (angVelWrtN frame0) + w
angVelWrtN (RFrame frame0 (RotCoord q) _) = (angVelWrtN frame0) + partialV q (SExpr time)

-- | partial derivative, if the argument is time this will be the full derivative
partial :: Sca -> Sca -> Sca
partial _ SZero      = error "partial taken w.r.t. non-symbolic"
partial _ SOne       = error "partial taken w.r.t. non-symbolic"
partial _ (SMul _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SDiv _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SAdd _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SSub _ _) = error "partial taken w.r.t. non-symbolic"
partial _ (SNeg _)   = error "partial taken w.r.t. non-symbolic"
partial _ (SDot _ _) = error "partial taken w.r.t. non-symbolic"
partial SZero _ = SZero
partial SOne _ = SZero
partial (SDot b x) arg = SDot b (partial x arg)
partial (SNeg x) arg = -(partial x arg)
partial (SMul x y) arg = x*y' + x'*y
  where
    x' = partial x arg
    y' = partial y arg
partial (SDiv x y) arg = x'/y - x/(y*y)*y'
  where
    x' = partial x arg
    y' = partial y arg
partial (SAdd x y) arg = (partial x arg) + (partial y arg)
partial (SSub x y) arg = (partial x arg) - (partial y arg)
partial (SExpr x) (SExpr arg)
  | isVal 0 ret = SZero
  | isVal 1 ret = SOne
  | otherwise = ret
  where
    ret = SExpr $ head (runDeriv x [arg])

-- | partial derivative, if the argument is time this will be a full derivative
--   but will not apply the golden rule of vector differentiation
partialV :: Vec -> Sca -> Vec
partialV (Vec hm) arg = removeZeros $ Vec (HM.map (flip partial arg) hm)


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
time :: Expr Dvda.Z Double
time = sym "t"

