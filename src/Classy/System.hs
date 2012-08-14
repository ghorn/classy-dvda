{-# OPTIONS_GHC -Wall #-}

module Classy.System ( getCMPos
                     , generalizedForce
                     , generalizedEffectiveForce
                     , kaneEq
                     , kaneEqs
                     ) where

import Classy.Differentiation
import Classy.VectorMath
import Classy.Types

-- | get center of mass position
getCMPos :: Body -> Point
getCMPos (Particle _ p) = p
getCMPos (RigidBody _ _ p _) = p


generalizedEffectiveForce :: Sca -> Body -> Sca
generalizedEffectiveForce gspeed (Particle mass pos) =
  partialV vel gspeed `dot` scale mass accel
  where
    vel = ddtNp pos
    accel = ddtN vel
generalizedEffectiveForce gspeed (RigidBody mass inertia pos frame) = translational + rotational
  where
    w = angVelWrtN frame
    vel = ddtNp pos
    accel = ddtN vel
    translational = partialV vel gspeed `dot` scale mass accel
    rotational = partialV w gspeed `dot` effectiveTorque
    effectiveTorque = inertia `dyadicDot` ddtN w
                      + w `cross` (inertia `dyadicDot` w)
--                      + mass `scale` (pos_bp `cross`accel)
-- can only take moment of effective force about cm right now

generalizedForce :: Sca -> Body -> Forces -> Moments -> Sca
generalizedForce gspeed (Particle _ pos) (Forces forces) _ =
  partialV vel gspeed `dot` sum (map snd forces)
  where
    vel = ddtNp pos
generalizedForce gspeed (RigidBody _ _ pos frame) (Forces forces) (Moments moments) =
  translational + rotational
  where
    w = angVelWrtN frame
    vel = ddtNp pos
    translational = partialV vel gspeed `dot` sum (map snd forces)
    rotational = partialV w gspeed `dot`
                 (sum moments + sum (map (\(p,f) -> subtractPoints p pos `cross` f) forces))
    
kaneEq :: [(Body, Forces, Moments)] -> Sca -> Equation Sca
kaneEq bft gspeed
  | not (isSpeed gspeed) = error $ "kaneEq given something that is not a generalized speed: " ++ show gspeed
  | otherwise =
    Equation
    (sum $ map (\(body,forces,moments) -> generalizedForce gspeed body forces moments) bft)
    EQ
    (sum $ map (\(body,_,_) -> generalizedEffectiveForce gspeed body) bft)

-- | run kanes equations for given generalized speeds
kaneEqs :: [(Body,Forces,Moments)] -> [Sca] -> [Equation Sca]
kaneEqs bodies = map (kaneEq bodies)
