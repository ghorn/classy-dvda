{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module Sym2 ( ddt
            , ddtV
            , partial
            , partialV
            , fOfT
            , go
            , ddtN'
            ) where

import qualified Data.HashMap.Lazy as HM

import Types
import Dvda hiding ( scale )
import Dvda.Expr ( symDependent, isVal )

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
ddtN' :: Frame -> (Basis, Sca) -> Vec
ddtN' f (basis@(Basis bf _), sca) = ddtBf + wCrossV
  where
    ddtBf = scaleBasis (ddt sca) basis
    wCrossV = w `cross` (scaleBasis sca basis)
      where
        w = (angVelWrtN bf) - (angVelWrtN f)
ddtN' f (basis@(Cross bf0 bf1), sca) = (scaleBasis (ddt sca) basis) + wCrossV
  where
    wCrossV = w `cross` (scaleBasis sca basis)
      where
        w = (angVelWrtN bf) - (angVelWrtN f)

ddtV :: Frame -> Vec -> Vec
ddtV = undefined
--ddtV _ VZero = VZero
--ddtV f (VCross vx vy) = ((ddtV f vx) `cross` vy) + (vx `cross`  (ddtV f vy))
--ddtV f (VAdd   vx vy) = (ddtV f vx) + (ddtV f vy)
--ddtV f (VSub   vx vy) = (ddtV f vx) - (ddtV f vy)
--ddtV f (VNeg   vx)    = - (ddtV f vx)
--ddtV f v@(VBasis (Basis bf xyz) sca) 
--  | f == bf = ddtN
--  | otherwise = ddtN + wCrossV
--  where
--    ddtN
--      | ddtS == SZero = VZero
--      | otherwise     = VBasis (Basis bf xyz) ddtS
--      where ddtS = ddt sca
--    wCrossV = w `cross` v
--      where
--        w = (angVelWrtN bf) - (angVelWrtN f)

--------------------------------------------------------------------
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

expandRotations :: Frame -> [Frame]
expandRotations f@(NewtonianFrame _) = [f]
expandRotations f@(RFrame f' _ _) = expandRotations f' ++ [f]

angVelWrtN :: Frame -> Vec
angVelWrtN (NewtonianFrame _) = zeroVec
angVelWrtN (RFrame frame0 rot _) = (angVelWrtN frame0) + (angVel frame0 rot)
  where
    angVel :: Frame -> Rotation -> Vec
    angVel = undefined
--    angVel f (RotCoord VBasis bf xyz q) = VBasis bf xyz (ddt q)
--    angVel _ (RotCoord bf VZero) = VZero
--    angVel f (RotCoord (VNeg x)) = negate $ angVel f (RotCoord x)
--    angVel f (RotCoord (VAdd _ _)) = error "implement angVel RotCord VAdd"
--    angVel f (RotCoord (VSub _ _)) = error "implement angVel RotCord VSub"
--    angVel f (RotCoord (VCross _ _)) = error "implement angVel RotCord VCross"
----    angVel _ (WRot w) = w

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
      
      n_v_p = ddtV n r_n02p
      n_a_p = ddtV n n_v_p
  
  print r_n02p
  print n_v_p
  print (partialV n_v_p q')
  print n_a_p
