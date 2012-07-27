{-# OPTIONS_GHC -Wall #-}

module Examples ( simple
                , blah
                ) where

import System
import Frames
import Types

simple :: IO ()
simple = do
  let n = NewtonianFrame "N"

      jx = param "Jx"
      jy = param "Jy"
      jz = param "Jz"

      wx = speed "wx"
      wy = speed "wy"
      wz = speed "wz"

      mx = param "Tx"
      my = param "Ty"
      mz = param "Tz"
      torque = Torque $ scaleBasis mx (Basis b X) + scaleBasis my (Basis b Y) + scaleBasis mz (Basis b Z)
--      torque = Torque $ scaleBasis mx (Basis n X) + scaleBasis my (Basis n Y) + scaleBasis mz (Basis n Z)

      b = RFrame n (RotSpeed (wx,wy,wz)) "B"
      body = RigidBody 1 (simpleDyadic jx jy jz b) 0 b (Force 0) torque

  print body
  putStrLn "kane's eqs: "
  mapM_ print $ kaneEqs [body] [wx, wy, wz]
      

blah :: IO ()
blah = do
  let q = coord "q"
      q' = ddt q

      n = NewtonianFrame "N"
      b = rotZ n q "B"

      len = param "r"
      --len = 1.3

      r_n02p = scaleBasis len (Basis b X)

      v_pn = ddtN r_n02p
      a_pn = ddtN v_pn

      nx = scaleBasis 1 (Basis n X)

      someParticle = Particle 1.0 r_n02p (Force 0)

      wx = speed "wx"
      wy = speed "wy"
      wz = speed "wz"
      someRigidBody = RigidBody 1 (simpleDyadic 2 3 5 b) r_n02p b (Force 0) (Torque 0)

      
  putStrLn $ "r_n02p:            " ++ show r_n02p
  putStrLn $ "v_pn:              " ++ show v_pn
  putStrLn $ "partialV v_pn q':  " ++ show (partialV v_pn q')
  putStrLn $ "a_pn:              " ++ show a_pn
  putStrLn $ "dot r_n02p v_pn:   " ++ show (dot r_n02p v_pn)
  putStrLn $ "dot a_pn nx:       " ++ show (dot a_pn nx)

  putStrLn $ "Particle: " ++ show someParticle
  putStrLn $ "generalized force: " ++ show (generalizedForce q' someParticle)
  putStrLn $ "generalized effective force: " ++ show (generalizedEffectiveForce q' someParticle)

  putStrLn "------------------------------"
  putStrLn $ "Rigid Body: " ++ show someRigidBody
  putStrLn $ "generalized force: " ++ show (generalizedForce q' someRigidBody)
  putStrLn $ "generalized effective force: " ++ show (generalizedEffectiveForce q' someRigidBody)

  putStrLn "----------------------------"
  putStrLn "kane's eqs: "
--  print $ kaneEq [someParticle, someRigidBody] q'
  print $ kaneEq [someRigidBody] q'
  print $ kaneEq [someRigidBody] wx
  print $ kaneEq [someRigidBody] wy
  print $ kaneEq [someRigidBody] wz


