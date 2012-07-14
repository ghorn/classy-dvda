{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module Sym2 ( ddt
            , ddtV
            , partial
            , partialV
            , fOfT
            , go
            ) where

import Data.HashMap.Lazy ( HashMap )
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
ddtV :: Frame -> Vec -> Vec
ddtV _ VZero = VZero
ddtV f (VCross vx vy) = ((ddtV f vx) `cross` vy) + (vx `cross`  (ddtV f vy))
ddtV f (VAdd   vx vy) = (ddtV f vx) + (ddtV f vy)
ddtV f (VSub   vx vy) = (ddtV f vx) - (ddtV f vy)
ddtV f (VNeg   vx)    = - (ddtV f vx)
ddtV f v@(VBasis (Basis bf xyz) sca) 
  | f == bf = ddtN
  | otherwise = ddtN + wCrossV
  where
    ddtN
      | ddtS == SZero = VZero
      | otherwise     = VBasis (Basis bf xyz) ddtS
      where ddtS = ddt sca
    wCrossV = w `cross` v
      where
        w = (angVelWrtN bf) - (angVelWrtN f)

--------------------------------------------------------------------
simplifyVecAddSub :: Vec -> Vec
simplifyVecAddSub v = sum $ map (\(x,n) -> scale (fromIntegral n) x) (HM.toList counts)
  where
    counts = countPlusMinus HM.empty 1 v

countPlusMinus :: HashMap Vec Int -> Int -> Vec -> HashMap Vec Int
countPlusMinus hm sign (VNeg x)   = countPlusMinus hm (-sign) x
countPlusMinus hm sign (VAdd x y) = HM.unionWith (+) (countPlusMinus hm sign x) (countPlusMinus hm sign y)
countPlusMinus hm sign (VSub x y) = HM.unionWith (+) (countPlusMinus hm sign x) (countPlusMinus hm (-sign) y)
countPlusMinus hm sign v = HM.insertWith (+) v sign hm

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
angVelWrtN (NewtonianFrame _) = VZero
angVelWrtN (RFrame frame0 rot _) = (angVelWrtN frame0) + (angVel frame0 rot)
  where
    angVel :: Frame -> Rotation -> Vec
    angVel f (RotCoord VBasis bf xyz q) = VBasis bf xyz (ddt q)
    angVel _ (RotCoord bf VZero) = VZero
    angVel f (RotCoord (VNeg x)) = negate $ angVel f (RotCoord x)
    angVel f (RotCoord (VAdd _ _)) = error "implement angVel RotCord VAdd"
    angVel f (RotCoord (VSub _ _)) = error "implement angVel RotCord VSub"
    angVel f (RotCoord (VCross _ _)) = error "implement angVel RotCord VCross"
--    angVel _ (WRot w) = w




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


-- partialV will FAIL with an error message if you are taking the derivitive 
-- w.r.t. a coordinate because for now (for simplicity) I assume that:
-- partial (f*bx>) p == (partial f p)*bx>
partialV :: Vec -> Sca -> Vec
partialV VZero _ = VZero
partialV (VCross vx vy) p = ((partialV vx p) `cross` vy) + (vx `cross` (partialV vy p))
partialV (VAdd   vx vy) p = (partialV vx p) + (partialV vy p)
partialV (VSub   vx vy) p = (partialV vx p) - (partialV vy p)
partialV (VNeg   v)     p = - (partialV v p)
partialV (VBasis basis sca) p
  | psp == SZero = VZero
  | otherwise    = VBasis basis psp
  where
    psp = partial sca p

--partialV :: Vec -> Sca -> Vec
--partialV _ (SCoord _) = error "partial velocity of vector wrt coordinate not yet supported"
--partialV VZero _ = VZero
--partialV (VCross vx vy) p = vSum (vCross (partialV vx p) vy) (vCross vx (partialV vy p))
--partialV (VAdd   vx vy) p = vSum (partialV vx p) (partialV vy p)
--partialV (VSub   vx vy) p = vSub (partialV vx p) (partialV vy p)
--partialV (VNeg   vx)    p = vNeg (partialV vx p)
--partialV (VBasis f xyz sca) p
--  | psp == SZero = VZero
--  | otherwise    = VBasis f xyz psp
--  where
--    psp = partial sca p


rotZ :: Frame -> Sca -> String -> Frame
rotZ f q name = RFrame f (RotCoord (VBasis (Basis f Z) q)) name

-----------------------------------------------------------------------------------------
----coord :: String -> Coord
--type Mass = Sca
--data Particle = ParticlePos Mass Vec
--              | ParticleVel Mass Vec Vec
--
--particle :: Double -> Vec -> Particle

fOfT :: String -> Sca
fOfT name = SExpr $ symDependent name time

--basis :: Frame -> XYZ -> Sca -> Vec
--basis f@(NewtonianFrame _) xyz s = VBasis [(f,xyz)] s
--basis f@(RFrame _) xyz s = VBasis [(f,xyz)] s

go :: IO ()
go = do
  let q = fOfT "q"
      q' = ddt q
      
      n = NewtonianFrame "N"
      b = rotZ n q "B"
      
      len = 1.3
      r_n02p = VBasis (Basis b X) len
      
      n_v_p = ddtV n r_n02p
      n_a_p = ddtV n n_v_p
  
  print r_n02p
  print n_v_p
  print (partialV n_v_p q')
  print n_a_p
