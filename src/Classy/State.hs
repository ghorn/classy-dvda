{-# OPTIONS_GHC -Wall #-}
{-# Language DoAndIfThenElse #-}

module Classy.State ( ClassyState(..)
                    -- * scalars
                    , addParam, addAction, addCoord, addSpeed
                    , derivIsSpeed
                    , setDeriv
                    -- * bases
                    , newtonianBases
                    , rotXYZ, rotX, rotY, rotZ
                    -- * bodies
                    , addParticle
                    , addForce

                    , emptyClassyState
                    , kanes
                    , run
                    ) where

import Control.Monad.State.Lazy
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS

import qualified Classy.Convenience as CC
import Classy.Differentiation ( ddt )
import Classy.System
import Classy.Types
import Classy.VectorMath ( scaleBasis )

data DCM = SimpleRot XYZ Sca deriving Show

data ClassyState = ClassyState { csCoords :: HashSet Sca
                               , csSpeeds :: HashSet Sca
                               , csParams :: HashSet Sca
                               , csActions :: HashSet Sca
                               , csCoordDerivs :: HashMap Sca Sca
                               , csBases :: HashSet Bases
                               , csDots :: HashMap (Basis, Basis) Sca
                               , csParticles :: HashMap (Sca,Point) Forces'
                               } deriving Show

emptyClassyState :: ClassyState
emptyClassyState = ClassyState { csCoords = HS.empty
                               , csSpeeds = HS.empty
                               , csParams = HS.empty
                               , csActions = HS.empty
                               , csCoordDerivs = HM.empty
                               , csBases = HS.empty
                               , csDots = HM.empty
                               , csParticles = HM.empty
                               }

data Body' = Particle' Sca Point deriving (Eq, Show)
data Forces' = Forces' [(Point,Vec)] deriving Show

addParam :: String -> State ClassyState Sca
addParam name = do
  cs <- get
  let p = CC.param name
  put $ cs{ csParams = HS.insert p (csParams cs) }
  return p

addAction :: String -> State ClassyState Sca
addAction name = do
  cs <- get
  let u = CC.param name
  put $ cs{ csActions = HS.insert u (csActions cs) }
  return u

addCoord :: String -> State ClassyState Sca
addCoord name = do
  cs <- get
  let c = CC.coord name
  put $ cs{ csCoords = HS.insert c (csCoords cs) }
  return c

addSpeed :: String -> State ClassyState Sca
addSpeed name = do
  cs <- get
  let s = CC.speed name
  put $ cs{ csSpeeds = HS.insert s (csSpeeds cs) }
  return s

setDeriv :: Sca -> Sca -> State ClassyState ()
setDeriv c c' =
  if not (isCoord c)
  then error "you can only set the derivative of a coordinate with setDeriv"
  else do
    cs <- get
    let err = error $ "setDeriv: the derivative of " ++ show c ++ "is already set"
        newCoordDerivs = HM.insertWith err c c' (csCoordDerivs cs)
    put $ cs{ csCoordDerivs = newCoordDerivs }
                  
derivIsSpeed :: Sca -> State ClassyState ()
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

addParticle :: Sca -> Point -> State ClassyState Body'
addParticle mass position = do
  cs <- get
  let err = error $ "error: you tried to add an existing particle with \"addParticle\""
  put $ cs{ csParticles = HM.insertWith err (mass,position) (Forces' []) (csParticles cs) }
  return $ Particle' mass position

addForce :: Body' -> Vec -> State ClassyState ()
addForce p@(Particle' mass pos) force = do
  cs <- get
  let newForces = case HM.lookup (mass,pos) (csParticles cs) of
        Nothing -> error $ "addForce: called on unknown particle: " ++ show p
        Just (Forces' fs) -> Forces' ((pos,force):fs)
  put $ cs{ csParticles = HM.insert (mass,pos) newForces (csParticles cs) }

newtonianBases :: State ClassyState Bases
newtonianBases = return CC.newtonianBases

rotXYZ :: XYZ -> Bases -> Sca -> String -> State ClassyState Bases
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

rotX,rotY,rotZ :: Bases -> Sca -> String -> State ClassyState Bases
rotX = rotXYZ X
rotY = rotXYZ Y
rotZ = rotXYZ Z

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

kanes :: ClassyState -> [Equation Sca]
kanes cs = mapEqs (simplifyDcms (csDots cs)) unsimplifiedEqs
  where
    mapEqs :: (a -> b) -> [Equation a] -> [Equation b]
    mapEqs f' = map (fmapEq f')
      where
        fmapEq f (Equation x c y) = Equation (f x) c (f y)

    unsimplifiedEqs = kaneEqs bodies speeds
    bodies = map (\((mass,pos), Forces' fs) -> Particle mass pos (Forces fs))
             (HM.toList $ csParticles cs)
    speeds = HS.toList (csSpeeds cs)

run :: [Equation Sca]
run = kanes $ flip execState emptyClassyState $ do
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
  addForce basket (CC.zVec (mass*g) n)
  addForce basket (CC.zVec (-tension) b)
