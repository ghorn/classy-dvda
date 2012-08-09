{-# OPTIONS_GHC -Wall #-}

-- | API for this package
module Classy ( -- * refernce frames
                newtonianFrame
              , rotXYZ
              , rotX, rotY, rotZ
              , frameWithAngVel
                -- * needed to write type signatures
              , Sca
              , Vec
              , Frame
                -- * some primitives
              , XYZ(..)
              , coord
              , speed
              , param
                -- * vector/frame operations
              , xyzVec
              , basisVec
              , xVec, yVec, zVec
              , scaleBasis
                -- * vector/frame math
              , cross
              , dot
              , dyadDot
              , dyadicDot
              , scale
                -- * rigid bodies
              , simpleDyadic
                -- * differentiation
              , ddt
              , ddtN
              , partial
              , partialV
                -- * mechanical system and dynamics
              , Body(..)
              , Torque(..)
              , Force(..)
              , generalizedForce
              , generalizedEffectiveForce
              , kaneEq
              , kaneEqs
              ) where

import Dvda ( symDependent, sym )

import System
import Types
import VectorMath

-- | the one unique newtonian frame which all other frame are defined relative to
newtonianFrame :: Frame
newtonianFrame = NewtonianFrame

-- | define a new frame as x, y or z rotation about given frame, providing the name of the new frame
rotXYZ :: XYZ -> Frame -> Sca -> String -> Frame -- should check for generalized speeds/coords
rotXYZ xyz f0 q name
  | name `elem` ["N","n"] =
    error "don't name your frame \"N\" or \"n\", those are reserved for the unique newtonian frame"
  | otherwise = RotatedFrame f0 (RotCoord (scaleBasis q rotationBasis)) name
  where
    rotationBasis = Basis f0 xyz

-- | @c = frameWithAngVel n (wx,wy,wz) name@ defines a new frame @c@ named @name@
--  which is defined as having angular velocity @wx*cx>+ wy*cy> + wz*cz>@ with respect to frame @n@
frameWithAngVel :: Frame -> (Sca,Sca,Sca) -> String -> Frame
frameWithAngVel f0 (wx,wy,wz) name
  | coords /= [] =
    error $ "frameWithAngVel can't be given generalized coordinates " ++ show coords ++ " as speeds"
  | otherwise = RotatedFrame f0 (RotSpeed (wx,wy,wz)) name
  where
    coords = filter isCoord [wx,wy,wz]

-- | generalized coordinate
coord :: String -> Sca
coord name = SExpr (symDependent name time) (Just 0)

-- | generalized speed
speed :: String -> Sca
speed name = SExpr (symDependent name time) (Just 1)

-- | constant but symbolic parameter
param :: String -> Sca
param name = SExpr (sym name) Nothing

-- | convenience functions for calling rotXYZ
rotX,rotY,rotZ :: Frame -> Sca -> String -> Frame
rotX = rotXYZ X
rotY = rotXYZ Y
rotZ = rotXYZ Z

-- | express a vector as x/y/z components of a given frame
xyzVec :: (Sca,Sca,Sca) -> Frame -> Vec
xyzVec (sx,sy,sz) frame =
  scaleBasis sx (Basis frame X) +
  scaleBasis sy (Basis frame Y) +
  scaleBasis sz (Basis frame Z)

---- | create a basis from a frame and X or Y or Z
--basis :: Frame -> XYZ -> Basis
--basis = Basis

-- | create a vector from a scalar, a frame, and @X@ or @Y@ or @Z@
basisVec :: Sca -> Frame -> XYZ -> Vec
basisVec s frame xyz = scaleBasis s (Basis frame xyz)

-- | convenience functions for calling basisVec
xVec,yVec,zVec :: Sca -> Frame -> Vec
xVec s frame = basisVec s frame X
yVec s frame = basisVec s frame Y
zVec s frame = basisVec s frame Z


-- | specify the xx,yy,zz components of the moment of inertia dyadic in a given frame
--   all other components are zero
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
