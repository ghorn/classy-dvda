{-# OPTIONS_GHC -Wall #-}

module Classy.Convenience ( basisVec
                          , xVec, yVec, zVec
                          , simpleDyadic
                          , relativePoint
                          ) where

import Classy.Types
import Classy.VectorMath

---- | create a basis from a frame and X or Y or Z
--basis :: Bases -> XYZ -> Basis
--basis = Basis

-- | create a vector from a scalar, a frame, and @X@ or @Y@ or @Z@
basisVec :: Sca -> Bases -> XYZ -> Vec
basisVec s frame xyz = scaleBasis s (Basis frame xyz)

-- | convenience functions for calling basisVec
xVec,yVec,zVec :: Sca -> Bases -> Vec
xVec s frame = basisVec s frame X
yVec s frame = basisVec s frame Y
zVec s frame = basisVec s frame Z

-- | specify the xx,yy,zz components of the moment of inertia dyadic in a given frame
--   all other components are zero
simpleDyadic :: Sca -> Sca -> Sca -> Bases -> Dyadic
simpleDyadic jx jy jz frame =
  Dyadic ( (Dyad jx bx bx, Dyad  0 bx by, Dyad  0 bx bz)
         , (Dyad  0 by bx, Dyad jy by by, Dyad  0 by bz)
         , (Dyad  0 bz bx, Dyad  0 bz by, Dyad jz bz bz)
         )
  where
    bx = Basis frame X
    by = Basis frame Y
    bz = Basis frame Z

-- | specify a point relative to another point
relativePoint :: Point -> Vec -> Point
relativePoint = RelativePoint
