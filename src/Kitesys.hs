{-# OPTIONS_GHC -Wall #-}

module Kitesys where

import System
import Frames
import Types

rotX :: Frame -> Sca -> String -> Frame
rotX f0 q name = RFrame f0 (RotCoord (scaleBasis q rotationBasis)) name
  where
    rotationBasis = Basis f0 X

kitesys :: IO ()
kitesys = do
  let n = NewtonianFrame "N"

      rArm = param "R"
      delta = coord "d"

      carasolFrame = rotZ n delta "C"

      lineWy = speed "lwz"
      lineWz = speed "lwy"
      
      lineFrame = RFrame carasolFrame (RotSpeed (0,lineWy,lineWz)) "L"

      rLine = param "r"

      jx = param "Jx"
      jy = param "Jy"
      jz = param "Jz"

      wx = speed "wx"
      wy = speed "wy"
      wz = speed "wz"

      m = param "m"
      kiteFrame = RFrame n (RotSpeed (wx,wy,wz)) "K"
      r_n02k = scaleBasis rArm (Basis carasolFrame X) + scaleBasis rLine (Basis lineFrame X)

      fx = param "Fx"
      fy = param "Fy"
      fz = param "Fz"
      mx = param "Tx"
      my = param "Ty"
      mz = param "Tz"
      force =  Force  $ scaleBasis fx (Basis n X) + scaleBasis fy (Basis n Y) + scaleBasis fz (Basis n Z)
      torque = Torque $ scaleBasis mx (Basis n X) + scaleBasis my (Basis n Y) + scaleBasis mz (Basis n Z)

      kite = RigidBody m (simpleDyadic jx jy jz kiteFrame) r_n02k kiteFrame force torque

  print kite
  putStrLn "kane's eqs: "
  mapM_ print $ kaneEqs [kite] [wx,wy,wz,ddt delta]

--  putStrLn $ "r_n02p:            " ++ show r_n02p
--  putStrLn $ "v_pn:              " ++ show v_pn
--  putStrLn $ "partialV v_pn q':  " ++ show (partialV v_pn q')
--  putStrLn $ "a_pn:              " ++ show a_pn
--  putStrLn $ "dot r_n02p v_pn:   " ++ show (dot r_n02p v_pn)
--  putStrLn $ "dot a_pn nx:       " ++ show (dot a_pn nx)
--
--  putStrLn $ "Particle: " ++ show someParticle
--  putStrLn $ "generalized force: " ++ show (generalizedForce q' someParticle)
--  putStrLn $ "generalized effective force: " ++ show (generalizedEffectiveForce q' someParticle)
--
--  putStrLn "------------------------------"
--  putStrLn $ "Rigid Body: " ++ show someRigidBody
--  putStrLn $ "generalized force: " ++ show (generalizedForce q' someRigidBody)
--  putStrLn $ "generalized effective force: " ++ show (generalizedEffectiveForce q' someRigidBody)
--
--  putStrLn "----------------------------"
--  putStrLn "kane's eqs: "
----  print $ kaneEq [someParticle, someRigidBody] q'
--  print $ kaneEq [someRigidBody] q'
--  print $ kaneEq [someRigidBody] wx
--  print $ kaneEq [someRigidBody] wy
--  print $ kaneEq [someRigidBody] wz
