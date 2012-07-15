{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module Frames ( ddt
              , ddtN
              , partial
              , partialV
              , fOfT
              , go
              ) where

import qualified Data.HashMap.Lazy as HM

import Dvda hiding ( scale )
import Dvda.Expr ( symDependent, isVal )

import Types

time :: Expr Z Double
time = sym "t"

ddt :: Sca -> Sca
ddt (SExpr x)
  | isVal 0 ret = SZero
  | isVal 1 ret = SOne
  | otherwise = SExpr $ ret
  where
    ret = head $ runDeriv x [time]
ddt _ = SZero

-- | time derivative in a rotating frame using golden rule of vector differentiation
ddtN :: Vec -> Vec
ddtN (Vec hm0) = removeZeros $ sum $ map ddtN' (HM.toList hm0)
  where
    ddtN' :: (Basis, Sca) -> Vec
    ddtN' (basis, sca) = scaleBasis (ddt sca) basis + ddtNBasis basis
      where
        ddtNBasis :: Basis -> Vec
        ddtNBasis b@(Basis bf _) = (angVelWrtN bf) `cross` (scaleBasis 1 b)
        ddtNBasis (Cross bf0 bf1) = ddtN v0 `cross` v1 + v0 `cross` ddtN v1
          where
            v0 = scaleBasis 1 bf0
            v1 = scaleBasis 1 bf1


--------------------------------------------------------------------
angVelWrtN :: Frame -> Vec
angVelWrtN (NewtonianFrame _) = zeroVec
angVelWrtN (RFrame frame0 (RotCoordSpeed _ w) _) = (angVelWrtN frame0) + w
angVelWrtN (RFrame frame0 (RotSpeed w) _)        = (angVelWrtN frame0) + w
angVelWrtN (RFrame frame0 (RotCoord q) _)        = (angVelWrtN frame0) + partialV q (SExpr time)

--minRot :: Frame -> Frame -> Rotation
--minRot fx fy = blah
--  where
--    match (x:xs) (y:ys)
--      | x == y = match xz ys
--      | otherwise = 
--    
--    expandRots f@(NewtonianFrame name) = [f]
--    expandRots f@(RFrame f' rot name) = expandRots f' ++ [f]

--minimalRotation (NewtonianFrame n) x

--expandRotations :: Frame -> [Frame]
--expandRotations f@(NewtonianFrame _) = [f]
--expandRotations f@(RFrame f' _ _) = expandRotations f' ++ [f]

partial :: Sca -> Sca -> Sca
partial SZero _ = SZero
partial SOne _ = SZero
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
  | otherwise = SExpr ret
  where
    ret = head (runDeriv x [arg])
partial _ SZero      = error "unsafe partial intercept!!"
partial _ SOne       = error "unsafe partial intercept!!"
partial _ (SMul _ _) = error "unsafe partial intercept!!"
partial _ (SDiv _ _) = error "unsafe partial intercept!!"
partial _ (SAdd _ _) = error "unsafe partial intercept!!"
partial _ (SSub _ _) = error "unsafe partial intercept!!"
partial _ (SNeg _)   = error "unsafe partial intercept!!"

partialV :: Vec -> Sca -> Vec
partialV (Vec hm) arg = removeZeros $ Vec $ HM.map (flip partial arg) hm

---------------------------------------------------------------------------

rotZ :: Frame -> Sca -> String -> Frame
rotZ f q name = RFrame f (RotCoord (scaleBasis q (Basis f X))) name

-----------------------------------------------------------------------------------------
----coord :: String -> Coord
--type Mass = Sca
--data Particle = ParticlePos Mass Vec
--              | ParticleVel Mass Vec Vec
--
--particle :: Double -> Vec -> Particle

fOfT :: String -> Sca
fOfT name = SExpr $ symDependent name time

go :: IO ()
go = do
  let q = fOfT "q"
      q' = ddt q
      
      n = NewtonianFrame "N"
      b = rotZ n q "B"
      
      len = 1.3
      r_n02p = scaleBasis len (Basis b X)
      
      n_v_p = ddtN r_n02p
      n_a_p = ddtN n_v_p
  
  print r_n02p
  print n_v_p
  print (partialV n_v_p q')
  print n_a_p
