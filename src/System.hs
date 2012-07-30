{-# OPTIONS_GHC -Wall #-}

module System ( Body(..)
              , Torque(..)
              , Force(..)
              , generalizedForce
              , generalizedEffectiveForce
              , kaneEq
              , kaneEqs
              ) where

import VectorMath
import Types

data Force = Force Vec deriving Show
data Torque = Torque Vec deriving Show

data Body = Particle
            Sca --  mass
            Vec --  position from N0 to CM
            Force --  forces on the body
          | RigidBody
            Sca --  mass
            Dyadic --  inertia dyadic
            Vec --  position from N0 to CM
            Frame --  reference frame attached to the rigid body (for getting angular velocity)
            Force --  forces on the body
            Torque --  torque on the body
--data Body = Particle
--            Sca -- ^ mass
--            Vec -- ^ position from N0 to CM
--            Force -- ^ forces on the body
--          | RigidBody
--            Sca -- ^ mass
--            Dyadic -- ^ inertia dyadic
--            Vec -- ^ position from N0 to CM
--            Frame -- ^ reference frame attached to the rigid body (for getting angular velocity)
--            Force -- ^ forces on the body
--            Torque -- ^ torque on the body

instance Show Body where
  show (Particle mass pos (Force f)) = unlines [ "Particle"
                                               , "mass: " ++ show mass
                                               , "position: " ++ show pos
                                               , "force: " ++ show f
                                               ]
  show (RigidBody mass inertia pos frame (Force force) (Torque torque)) =
    unlines [ "RigidBody"
            , "mass: " ++ show mass
            , "inertia: " ++ show inertia
            , "position: " ++ show pos
            , "angular velocity in newtonian frame: " ++ show (angVelWrtN frame)
            , "force: " ++ show force
            , "torque: " ++ show torque
            ]

generalizedEffectiveForce :: Sca -> Body -> Sca
generalizedEffectiveForce gspeed (Particle mass pos _) = (partialV vel gspeed) `dot` (scale mass accel)
  where
    vel = ddtN pos
    accel = ddtN vel
generalizedEffectiveForce gspeed (RigidBody mass inertia pos frame _ _) = translational + rotational
  where
    w = angVelWrtN frame
    vel = ddtN pos
    accel = ddtN vel
    translational = (partialV vel gspeed) `dot` (scale mass accel)
    rotational = (partialV w gspeed) `dot` effectiveTorque
    effectiveTorque = inertia `dyadicDot` (ddtN w) + w `cross` (inertia `dyadicDot` w) + mass `scale` (pos `cross`accel)

generalizedForce :: Sca -> Body -> Sca
generalizedForce gspeed (Particle _ pos (Force force)) = (partialV vel gspeed) `dot` force
  where
    vel = ddtN pos
generalizedForce gspeed (RigidBody _ _ pos frame (Force force) (Torque torque)) = translational + rotational
  where
    w = angVelWrtN frame
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
