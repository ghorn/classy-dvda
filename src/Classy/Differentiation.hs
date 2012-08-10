{-# OPTIONS_GHC -Wall #-}

module Classy.Differentiation ( ddt
                              , ddtN
                              , ddtNp
                              , ddtF
                              , partial
                              , partialV
                              , angVelWrt
                              , angVelWrtN
                              ) where

import qualified Data.HashMap.Lazy as HM
import Debug.Trace

import Dvda

import Classy.Types
import Classy.Utils ( lastCommonBases )
import Classy.VectorMath

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

-- | Angular velocity of a frame w.r.t the Newtonian frame
angVelWrtN :: Bases -> Vec
angVelWrtN frame = angVelWrt frame NewtonianBases 

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
