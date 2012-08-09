{-# OPTIONS_GHC -Wall #-}

module System ( Body(..)
              , Torque(..)
              , Force(..)
              , getCMPos
              , generalizedForce
              , generalizedEffectiveForce
              , kaneEq
              , kaneEqs
              ) where

--import Data.HashSet ( HashSet )
--import qualified Data.HashSet as HS
--import qualified Data.HashMap.Lazy as HM

import VectorMath
import Types

data Force = Force Vec deriving Show
data Torque = Torque Vec deriving Show

data Body = Particle
            Sca --  mass
            Point --  position from N0 to CM
            Force --  forces on the body
          | RigidBody
            Sca --  mass
            Dyadic --  inertia dyadic
            Point --  position from N0 to CM
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

-- | get center of mass position
getCMPos :: Body -> Point
getCMPos (Particle _ p _) = p
getCMPos (RigidBody _ _ p _ _ _) = p

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
generalizedEffectiveForce gspeed (Particle mass pos _) = partialV vel gspeed `dot` scale mass accel
  where
    vel = ddtNp pos
    accel = ddtN vel
generalizedEffectiveForce gspeed (RigidBody mass inertia pos frame _ _) = translational + rotational
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

generalizedForce :: Sca -> Body -> Sca
generalizedForce gspeed (Particle _ pos (Force force)) = partialV vel gspeed `dot` force
  where
    vel = ddtNp pos
generalizedForce gspeed (RigidBody _ _ pos frame (Force force) (Torque torque)) = translational + rotational
  where
    w = angVelWrtN frame
    vel = ddtNp pos
    translational = partialV vel gspeed `dot` force
    rotational    = partialV w gspeed `dot` torque

kaneEq :: [Body] -> Sca -> Equation Sca
kaneEq bodies gspeed
  | not (isSpeed gspeed) = error $ "kaneEq given something that is not a generalized speed: " ++ show gspeed
  | otherwise = Equation
                (sum $ map (generalizedForce gspeed) bodies)
                EQ
                (sum $ map (generalizedEffectiveForce gspeed) bodies)

--class GetSpeeds a where
--  getSpeeds :: a -> HashSet Sca
--
--instance GetSpeeds Body where
--  getSpeeds (Particle mass pos (Force force)) = HS.unions [getSpeeds mass, getSpeeds pos, getSpeeds force]
--  getSpeeds (RigidBody mass inertia pos frame (Force force) (Torque torque)) =
--    HS.unions [ getSpeeds mass
--              , getSpeeds inertia
--              , getSpeeds pos
--              , getSpeeds frame
--              , getSpeeds force
--              , getSpeeds torque
--              ]
--
--instance GetSpeeds Vec where
--  getSpeeds (Vec hm) = HS.unions $ map getSpeeds (HM.elems hm)
--
--instance GetSpeeds Sca where
--  getSpeeds = foldSca f HS.empty
--    where
--      f s acc = if isSpeed s then HS.insert s acc else acc
--
--instance GetSpeeds Frame where
--  getSpeeds NewtonianFrame = HS.empty
--  getSpeeds (RotatedFrame frame rot _) = HS.union (getSpeeds frame) (getSpeeds rot)
--
--instance GetSpeeds Rotation where
--  getSpeeds (RotSpeed (wx,wy,wz)) = HS.unions $ map getSpeeds [wx,wy,wz]
--  getSpeeds (RotCoord v) = getSpeeds v
--
--instance GetSpeeds Dyadic where
--  getSpeeds (Dyadic ((xx,xy,xz), (yx,yy,yz), (zx,zy,zz))) = HS.unions $ map getSpeeds [xx,xy,xz,yx,yy,yz,zx,zy,zz]
--
--instance GetSpeeds Dyad where
--  getSpeeds (Dyad s _ _) = getSpeeds s
--
---- | run kanes equations, automatically discovering all generalized speeds
--kaneEqs' :: [Body] -> [Equation Sca]
--kaneEqs' bodies = kaneEqs' bodies speeds
--  where
--    speeds = HS.toList $ HS.unions $ map getSpeeds bodies

-- | run kanes equations for given generalized speeds
kaneEqs :: [Body] -> [Sca] -> [Equation Sca]
kaneEqs bodies = map (kaneEq bodies)
