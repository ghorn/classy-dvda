{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module System ( rotZ
              , fOfT
              , go
              ) where

import Dvda ( symDependent )

import Frames
import Types
                

fOfT :: String -> Sca
fOfT name = SExpr $ symDependent name time

rotZ :: Frame -> Sca -> String -> Frame
rotZ f q name = RFrame f (RotCoord (scaleBasis q (Basis f X))) name

-----------------------------------------------------------------------------------------
----coord :: String -> Coord
--type Mass = Sca
--data Particle = ParticlePos Mass Vec
--              | ParticleVel Mass Vec Vec
--
--particle :: Double -> Vec -> Particle

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

