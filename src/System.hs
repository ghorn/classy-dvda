{-# OPTIONS_GHC -Wall #-}

module System ( rotX
              , rotY
              , rotZ
              , go
              , woo
              , generalizedForce
              , generalizedEffectiveForce
              , kaneEq
              , kaneEqs
              ) where

import Frames
import Types

rotXYZ :: XYZ -> Frame -> Sca -> String -> Frame
rotXYZ xyz f0 q name = RFrame f0 (RotCoord (scaleBasis q rotationBasis)) name
  where
    rotationBasis = Basis f0 xyz

rotX,rotY,rotZ :: Frame -> Sca -> String -> Frame
rotX = rotXYZ X
rotY = rotXYZ Y
rotZ = rotXYZ Z

data Force = Force Vec deriving Show
data Torque = Torque Vec deriving Show

simpleDyadic :: Sca -> Sca -> Sca -> Frame -> Dyadic
simpleDyadic jx jy jz frame =
  Dyadic ( (Dyad jx bx bx, Dyad  0 bx by, Dyad  0 bx bz)
         , (Dyad  0 by bx, Dyad jy by by, Dyad  0 by bz)
         , (Dyad  0 bz bx, Dyad  0 bz by, Dyad jz bz bz)
         )
  where
    bx = Basis frame X
    by = Basis frame Y
    bz = Basis frame Z

data Body = Particle
            Sca -- ^ mass
            Vec -- ^ position from N0 to CM
            Force -- ^ forces on the body
          | RigidBody
            Sca -- ^ mass
            Dyadic -- ^ inertia dyadic
            Vec -- ^ position from N0 to CM
            Vec -- ^ angular velocity
            Force -- ^ forces on the body
            Torque -- ^ torque on the body

instance Show Body where
  show (Particle mass pos (Force f)) = unlines [ "Particle"
                                               , "mass: " ++ show mass
                                               , "position: " ++ show pos
                                               , "force: " ++ show f
                                               ]
  show (RigidBody mass inertia pos omega (Force force) (Torque torque)) =
    unlines [ "RigidBody"
            , "mass: " ++ show mass
            , "inertia: " ++ show inertia
            , "position: " ++ show pos
            , "angular velocity: " ++ show omega
            , "force: " ++ show force
            , "torque: " ++ show torque
            ]

generalizedEffectiveForce :: Sca -> Body -> Sca
generalizedEffectiveForce gspeed (Particle mass pos _) = (partialV vel gspeed) `dot` (scale mass accel)
  where
    vel = ddtN pos
    accel = ddtN vel
generalizedEffectiveForce gspeed (RigidBody mass inertia pos w _ _) = translational + rotational
  where
    vel = ddtN pos
    accel = ddtN vel
    translational = (partialV vel gspeed) `dot` (scale mass accel)
    rotational = (partialV w gspeed) `dot` effectiveTorque
    effectiveTorque = inertia `dyadicDot` (ddtN w) + w `cross` (inertia `dyadicDot` w) + mass `scale` (pos `cross`accel)

generalizedForce :: Sca -> Body -> Sca
generalizedForce gspeed (Particle _ pos (Force force)) = (partialV vel gspeed) `dot` force
  where
    vel = ddtN pos
generalizedForce gspeed (RigidBody _ _ pos w (Force force) (Torque torque)) = translational + rotational
  where
    vel = ddtN pos
    translational = (partialV vel gspeed) `dot` force
    rotational    = (partialV w gspeed) `dot` torque

kaneEq :: [Body] -> Sca -> Equation Sca
kaneEq bodies gspeed
  | not (isSpeed gspeed) = error $ "kaneEq given something that is not a generalized speed: " ++ show gspeed
  | otherwise = Equation
                (sum $ map (generalizedForce gspeed) bodies)
                EQ
                (sum $ map (generalizedEffectiveForce gspeed) bodies)

kaneEqs :: [Body] -> [Sca] -> [Equation Sca]
kaneEqs bodies speeds = map (kaneEq bodies) speeds

go :: IO ()
go = do
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
      w = scaleBasis wx (Basis b X) + scaleBasis wy (Basis b Y) + scaleBasis wz (Basis b Z)
      someRigidBody = RigidBody 1 (simpleDyadic 2 3 5 b) r_n02p w (Force 0) (Torque 0)

      
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



woo :: IO ()
woo = do
  let n = NewtonianFrame "N"

      jx = param "Jx"
      jy = param "Jy"
      jz = param "Jz"

      wx = speed "wx"
      wy = speed "wy"
      wz = speed "wz"

      w = scaleBasis wx (Basis b X) + scaleBasis wy (Basis b Y) + scaleBasis wz (Basis b Z)
      b = RFrame n (RotSpeed (wx,wy,wz)) "B"
      someRigidBody = RigidBody 1 (simpleDyadic jx jy jz b) 0 w (Force 0) (Torque 0)

  print someRigidBody
  putStrLn "kane's eqs: "
  print $ kaneEq [someRigidBody] wx
  print $ kaneEq [someRigidBody] wy
  print $ kaneEq [someRigidBody] wz
