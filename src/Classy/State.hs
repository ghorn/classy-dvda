{-# OPTIONS_GHC -Wall #-}
{-# Language DoAndIfThenElse #-}

module Classy.State ( ClassySystem(..)
                    -- * scalars
                    , addParam, addAction, addCoord, addSpeed
                    , derivIsSpeed
                    , setDeriv
                    -- * bases
                    , newtonianBases
                    , rotXYZ, rotX, rotY, rotZ
                    , basesWithAngVel
                    -- * bodies
                    , addParticle
                    , addRigidBody
                    , addForce
                    , addForceAtCm
                    , addMoment

                    , runClassyState
                    , kanes
                    , run
                    ) where

import Control.Monad.State.Lazy
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS

import Dvda ( symDependent, sym )

import qualified Classy.Convenience as CC
import Classy.Differentiation ( ddt )
import Classy.System
import Classy.Types
import Classy.VectorMath ( scaleBasis )

data DCM = SimpleRot XYZ Sca deriving Show

data ClassySystem = ClassySystem { csCoords :: HashSet Sca
                                 , csSpeeds :: HashSet Sca
                                 , csParams :: HashSet Sca
                                 , csActions :: HashSet Sca
                                 , csCoordDerivs :: HashMap Sca Sca
                                 , csBases :: HashSet Bases
                                 , csDots :: HashMap (Basis, Basis) Sca
                                 , csBodies :: HashMap Body (Forces,Moments)
                                 , csNewtonianBases :: Maybe Bases
                                 } deriving Show

runClassyState :: State ClassySystem a -> ClassySystem
runClassyState = flip execState emptyClassySystem
  where
    emptyClassySystem = ClassySystem { csCoords = HS.empty
                                   , csSpeeds = HS.empty
                                   , csParams = HS.empty
                                   , csActions = HS.empty
                                   , csCoordDerivs = HM.empty
                                   , csBases = HS.empty
                                   , csDots = HM.empty
                                   , csBodies = HM.empty
                                   , csNewtonianBases = Nothing
                                   }

-- | the one unique newtonian frame which all other frame are defined relative to
newtonianBases :: State ClassySystem Bases
newtonianBases = do
  cs <- get
  case csNewtonianBases cs of
    Just _ -> error "newtonianBases: already set!"
    Nothing -> do
      let nb = NewtonianBases
      put $ cs{ csNewtonianBases = Just nb }
      return nb


addParam :: String -> State ClassySystem Sca
addParam name = do
  cs <- get
  let p = SExpr (sym name) Nothing
  put $ cs{ csParams = HS.insert p (csParams cs) }
  return p

addAction :: String -> State ClassySystem Sca
addAction name = do
  cs <- get
  let u = SExpr (sym name) Nothing
  put $ cs{ csActions = HS.insert u (csActions cs) }
  return u

addCoord :: String -> State ClassySystem Sca
addCoord name = do
  cs <- get
  let c = SExpr (symDependent name time) (Just 0)
  put $ cs{ csCoords = HS.insert c (csCoords cs) }
  return c

addSpeed :: String -> State ClassySystem Sca
addSpeed name = do
  cs <- get
  let s = SExpr (symDependent name time) (Just 1)
  put $ cs{ csSpeeds = HS.insert s (csSpeeds cs) }
  return s

setDeriv :: Sca -> Sca -> State ClassySystem ()
setDeriv c c' =
  if not (isCoord c)
  then error "you can only set the derivative of a coordinate with setDeriv"
  else do
    cs <- get
    let err = error $ "setDeriv: the derivative of " ++ show c ++ "is already set"
        newCoordDerivs = HM.insertWith err c c' (csCoordDerivs cs)
    put $ cs{ csCoordDerivs = newCoordDerivs }
                  
derivIsSpeed :: Sca -> State ClassySystem ()
derivIsSpeed c = do
  let s = ddt c
  if not (isCoord c)
  then error $ "derivIsSpeed given \"" ++ show s ++ "\" which is not a generalized coordinate"
  else do
    cs <- get
    let newSpeeds = if HS.member s (csSpeeds cs)
                    then error $ "derivIsSpeed: " ++ show s ++ "is already a generalized speed"
                    else HS.insert s (csSpeeds cs)

    put $ cs{ csSpeeds = newSpeeds }
    setDeriv c s

addParticle :: Sca -> Point -> State ClassySystem Body
addParticle mass position = do
  cs <- get
  let err = error $ "error: you tried to add an existing particle with \"addParticle\"\n"++show p
      p = Particle mass position
  put $ cs{ csBodies = HM.insertWith err p (Forces [], Moments []) (csBodies cs) }
  return p

addRigidBody :: Sca -> Dyadic -> Point -> Bases -> State ClassySystem Body
addRigidBody mass dyadic position bases = do
  cs <- get
  let err = error $ "error: you tried to add an existing rigid body with \"addRigidBody\"\n"++show rb
      rb = RigidBody mass dyadic position bases
  put $ cs{ csBodies = HM.insertWith err rb (Forces [], Moments []) (csBodies cs) }
  return rb

-- | add a force to a rigidy body to be applied at a given point
addForce :: Body -> Point -> Vec -> State ClassySystem ()
addForce p@(Particle _ _) pos force = do
  cs <- get
  let newForcesMoments = case HM.lookup p (csBodies cs) of
        Nothing -> error $ "addForce: called on unknown particle: " ++ show p
        Just (Forces fs,ts) -> (Forces ((pos,force):fs), ts)
  put $ cs{ csBodies = HM.insert p newForcesMoments (csBodies cs) }
addForce rb@(RigidBody _ _ _ _) pos force = do
  cs <- get
  let newForcesMoments = case HM.lookup rb (csBodies cs) of
        Nothing -> error $ "addForce: called on unknown rigid body: " ++ show rb
        Just (Forces fs, ts) -> (Forces ((pos,force):fs), ts)
  put $ cs{ csBodies = HM.insert rb newForcesMoments (csBodies cs) }

-- | add a force to a rigidy body to be applied at the body's center of mass
addForceAtCm :: Body -> Vec -> State ClassySystem ()
addForceAtCm b force = addForce b (getCMPos b) force

addMoment :: Body -> Vec -> State ClassySystem ()
addMoment p@(Particle _ _) _ =
  error $ "addMoment: called on particle: " ++ show p ++ ", can only call addMoment on a rigid body"
addMoment rb@(RigidBody _ _ _ _) moment = do
  cs <- get
  let newForcesMoments = case HM.lookup rb (csBodies cs) of
        Nothing -> error $ "addMoment: called on unknown rigid body: " ++ show rb
        Just (fs, Moments ts) -> (fs, Moments (moment:ts))
  put $ cs{ csBodies = HM.insert rb newForcesMoments (csBodies cs) }

-- should check for generalized speeds/coords
-- | define a new frame as x, y or z rotation about given frame, providing the name of the new frame
rotXYZ :: XYZ -> Bases -> Sca -> String -> State ClassySystem Bases
rotXYZ xyz b0 q name = do
  cs <- get
  let newBases' =
        if name `elem` (["N","n"] ++ map show (HS.toList (csBases cs)))
        then error $ "rot" ++ show xyz ++ " error: basis name \""++name++"\" has already been used"
        else RotatedBases b0 (RotCoord (scaleBasis q rotationBasis)) name
        where
          rotationBasis = Basis b0 xyz

      newBases =
        if HS.member newBases' (csBases cs)
        then error $ "rot" ++ show xyz ++ " error: you've defined a bases that already exists"
        else newBases'

      a = Basis b0
      b = Basis newBases

      (se,ce) = case q of (SExpr expr _) -> (SExpr (sin expr) Nothing, SExpr (cos expr) Nothing)
                          s -> error $ "rotXYZ got non SExpr value: " ++ show s
      newDots = HM.fromList $ case xyz of
        X -> [ ((a X, b X),  1), ((a Y, b X),   0), ((a Z, b X),  0)
             , ((a X, b Y),  0), ((a Y, b Y),  ce), ((a Z, b Y), se)
             , ((a X, b Z),  0), ((a Y, b Z), -se), ((a Z, b Z), ce)
             ]
        Y -> [ ((a X, b X), ce), ((a Y, b X), 0), ((a Z, b X), -se)
             , ((a X, b Y),  0), ((a Y, b Y), 1), ((a Z, b Y),   0)
             , ((a X, b Z), se), ((a Y, b Z), 0), ((a Z, b Z),  ce)
             ]
        Z -> [ ((a X, b X),  ce), ((a Y, b X), se), ((a Z, b X), 0)
             , ((a X, b Y), -se), ((a Y, b Y), ce), ((a Z, b Y), 0)
             , ((a X, b Z),   0), ((a Y, b Z),  0), ((a Z, b Z), 1)
             ]
      err = error "rotXyz adding dcm compenents that already exist: "
  
  put $ cs { csBases = HS.insert newBases (csBases cs)
           , csDots = HM.unionWith err newDots (csDots cs)
           }
  return newBases

-- | convenience functions for calling rotXYZ
rotX,rotY,rotZ :: Bases -> Sca -> String -> State ClassySystem Bases
rotX = rotXYZ X
rotY = rotXYZ Y
rotZ = rotXYZ Z

-- | @c = frameWithAngVel n (wx,wy,wz) name@ defines a new frame @c@ named @name@
--  which is defined as having angular velocity @wx*cx>+ wy*cy> + wz*cz>@ with respect to frame @n@
basesWithAngVel :: Bases -> (Sca,Sca,Sca) -> String -> Bases
basesWithAngVel f0 (wx,wy,wz) name
  | any isCoord [wx,wy,wz] =
    error $ "frameWithAngVel can't be given generalized coordinates " ++
    show (filter isCoord [wx,wy,wz]) ++ " as speeds"
  | otherwise = RotatedBases f0 (RotSpeed (wx,wy,wz)) name

simplifyDcms :: HashMap (Basis,Basis) Sca -> Sca -> Sca
simplifyDcms hm s@(SDot (b0,b1) x) =
  case (HM.lookup (b0,b1) hm, HM.lookup (b1,b0) hm) of
    (Nothing,Nothing) -> s
    (Just dotted,_) -> x*dotted
    (_,Just dotted) -> x*dotted
simplifyDcms hm (SNeg x) = negate (simplifyDcms hm x)
simplifyDcms hm (SAdd x y) = (simplifyDcms hm x) + (simplifyDcms hm y)
simplifyDcms hm (SSub x y) = (simplifyDcms hm x) - (simplifyDcms hm y)
simplifyDcms hm (SMul x y) = (simplifyDcms hm x) * (simplifyDcms hm y)
simplifyDcms hm (SDiv x y) = (simplifyDcms hm x) / (simplifyDcms hm y)
simplifyDcms _ s@(SExpr _ _) = s

kanes :: ClassySystem -> [Equation Sca]
kanes cs = mapEqs (simplifyDcms (csDots cs)) unsimplifiedEqs
  where
    mapEqs :: (a -> b) -> [Equation a] -> [Equation b]
    mapEqs f' = map (fmapEq f')
      where
        fmapEq f (Equation x c y) = Equation (f x) c (f y)

    unsimplifiedEqs = kaneEqs bodiesForcesMoments speeds
    bodiesForcesMoments = map (\(body, (fs,ts)) -> (body,fs,ts)) (HM.toList $ csBodies cs)
    speeds = HS.toList (csSpeeds cs)

run :: [Equation Sca]
run = kanes $ runClassyState $ do
  n <- newtonianBases

  q <- addCoord "q"
  r <- addCoord "r"
  derivIsSpeed q
  derivIsSpeed r

  mass <- addParam "m"
  g <- addParam "g"
  tension <- addParam "T"

  b <- rotY n q "B"

  let r_b_n0 = CC.relativePoint N0 (CC.zVec r b)

  basket <- addParticle mass r_b_n0
  addForceAtCm basket (CC.zVec (mass*g) n)
  addForceAtCm basket (CC.zVec (-tension) b)
