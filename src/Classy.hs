{-# OPTIONS_GHC -Wall #-}

-- | API for this package
module Classy ( -- * refernce frames
                newtonianBases
              , rotXYZ
              , rotX, rotY, rotZ
              , frameWithAngVel
                -- * needed to write type signatures
              , Sca
              , Vec
              , Bases
                -- * some primitives
              , XYZ(..)
              , Point(..)
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
              , getCMPos
                -- * differentiation
              , ddt
              , ddtN
              , ddtNp
              , ddtF
              , partial
              , partialV
                -- * mechanical system and dynamics
              , Body(..)
              , Torque(..)
              , Forces(..)
              , generalizedForce
              , generalizedEffectiveForce
              , kaneEq
              , kaneEqs
              ) where

import Classy.Convenience
import Classy.Differentiation
import Classy.System
import Classy.Types
import Classy.VectorMath
