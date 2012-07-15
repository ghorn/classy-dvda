{-# OPTIONS_GHC -Wall #-}

module System ( rotX
              , rotY
              , rotZ
              , param
              , go
              ) where

import Dvda ( sym )

import Frames
import Types
                

param :: String -> Sca
param name = SExpr $ sym name

rotXYZ :: XYZ -> Frame -> Sca -> String -> Frame
rotXYZ xyz f0 q name = RFrame f0 (RotCoord (scaleBasis q rotationBasis)) name
  where
    rotationBasis = Basis f0 xyz

rotX,rotY,rotZ :: Frame -> Sca -> String -> Frame
rotX = rotXYZ X
rotY = rotXYZ Y
rotZ = rotXYZ Z

go :: IO ()
go = do
  let q = fOfT "q"
      q' = ddt q
      
      n = NewtonianFrame "N"
      b = rotZ n q "B"
      
      len = param "r" -- 1.3

      r_n02p = scaleBasis len (Basis b X)

      nx = scaleBasis 1 (Basis n X)
      
      v_pn = ddtN r_n02p
      a_pn = ddtN v_pn
  
  putStrLn $ "r_n02p:            " ++ show r_n02p
  putStrLn $ "v_pn:              " ++ show v_pn
  putStrLn $ "partialV v_pn q':  " ++ show (partialV v_pn q')
  putStrLn $ "a_pn:              " ++ show a_pn
  print $ dot r_n02p v_pn
  print $ dot a_pn nx
  putStrLn ""
  print $ ddt (dot a_pn nx)
