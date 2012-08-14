{-# OPTIONS_GHC -Wall #-}

-- | API for this package
module Classy ( -- * refernce frames
                newtonianBases
              , rotXYZ
              , rotX, rotY, rotZ
              , basesWithAngVel
                -- * classy state transformer and convenience functions
              , ClassySystem
              , StateT
              , State
              , Identity
              , getSystem
              , getSystemT
              , liftIO
                -- * needed to write type signatures
              , Sca
              , Vec
              , Bases
                -- * some primitives
              , XYZ(..)
              , Point(N0)
              , addCoord
              , addSpeed
              , addParam
              , addAction
              , relativePoint
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
              , addRigidBody
              , addParticle
              , addMoment
              , addForce
              , kineticEnergy
              , generalizedForce
              , generalizedEffectiveForce
              , kanes
              ) where

import Classy.Convenience
import Classy.Differentiation
import Classy.State
import Classy.System
import Classy.Types
import Classy.VectorMath
