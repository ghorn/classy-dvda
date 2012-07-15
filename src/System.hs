{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module System ( rotX
              , rotY
              , rotZ
              , param
              , fOfT
              , go
              ) where

import Dvda ( sym, symDependent )

import Frames
import Types
                

fOfT :: String -> Sca
fOfT name = SExpr $ symDependent name time

param :: String -> Sca
param name = SExpr $ sym name

rotX :: Frame -> Sca -> String -> Frame
rotX f q name = RFrame f (RotCoord (scaleBasis q (Basis f X))) name

rotY :: Frame -> Sca -> String -> Frame
rotY f q name = RFrame f (RotCoord (scaleBasis q (Basis f Y))) name

rotZ :: Frame -> Sca -> String -> Frame
rotZ f q name = RFrame f (RotCoord (scaleBasis q (Basis f Z))) name

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
      v_pn = ddtN r_n02p
      a_pn = ddtN v_pn
  
  putStrLn $ "r_n02p:            " ++ show r_n02p
  putStrLn $ "v_pn:              " ++ show v_pn
  putStrLn $ "partialV v_pn q':  " ++ show (partialV v_pn q')
  putStrLn $ "a_pn:              " ++ show a_pn

