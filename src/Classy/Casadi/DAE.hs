{-# OPTIONS_GHC -Wall #-}

module Classy.Casadi.DAE ( DAE(..)
                         , mkDae
                         , writeDae
                         , run
                         ) where

import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Data.List ( intercalate )

import Dvda.FunGraph ( (:*)(..), toFunGraph )
import Dvda.Expr ( Expr(..), Sym(..) )
import Dvda.Codegen.PythonGen

import qualified Classy.Convenience as CC
import Classy.Differentiation ( ddt )
import Classy.State hiding ( run )
import Classy.Types
import Classy.Casadi.SXFunction
import Classy.Casadi.Bindings

import Python.Objects

newtype DAE = DAE SXFunction
instance ToPyObject DAE where toPyObject (DAE p) = toPyObject p
instance FromPyObject DAE where fromPyObject = fmap DAE . fromPyObject

toExpr :: Sca -> Expr Double
toExpr (SExpr e _) = e
toExpr (SNeg e) = negate (toExpr e)
toExpr (SAdd x y) = toExpr x + toExpr y
toExpr (SSub x y) = toExpr x - toExpr y
toExpr (SMul x y) = toExpr x * toExpr y
toExpr (SDiv x y) = toExpr x / toExpr y
toExpr e@(SDot _ _) = error $ "toExpr got something it couldn't convert to an Expr: " ++ show e

implicitError :: Num a => Equation a -> a
implicitError (Equation lhs EQ rhs) = lhs - rhs
implicitError (Equation _ ord _) = error $ "implicitError only works on EQ, not " ++ show ord

mkDae :: CasadiModule -> System -> IO DAE
mkDae casadiModule@(CasadiModule cm) cs = do
  let coords' = HS.toList (csCoords cs)
      coords = map toExpr coords'
      
      speeds' = HS.toList (csSpeeds cs)
      ddtSpeeds = map (toExpr . ddt) speeds'
      speeds = map toExpr speeds'

      actions = map toExpr $ HS.toList (csActions cs)
      params  = map toExpr $ HS.toList (csParams cs)
      odeError = map (toExpr . implicitError) (kanes cs) -- ++ (zipWith (-)

      ddtCoords = map f coords'
        where
          f c = case HM.lookup c (csCoordDerivs cs) of
            Nothing -> error $ "need to set derivative of " ++ show c
            Just d -> toExpr d

      dummyDdtCoords = map mangle ddtCoords
        where
          mangle (ESym (Sym name)) = ESym (Sym (name ++ "___mangled_lol"))
          mangle (ESym (SymDependent name k d)) = ESym (SymDependent (name ++ "___mangled_lol") k d)
          mangle e = error $ "dummyDdtCoords: can't mangle non-ESym: " ++ show e

  fg <- toFunGraph
        (coords :* speeds :* ddtSpeeds :* actions :* params :* dummyDdtCoords)
        (odeError :* (zipWith (-) dummyDdtCoords ddtCoords))

  ([coords'',  speeds'', ddtSpeeds'', actions'', params'', dummyDdtCoords''],
   [odeError'', ddtCoordError]) <- mkSXMs casadiModule fg

  sysErr <- veccat casadiModule [odeError'',ddtCoordError]
  x <- veccat casadiModule [coords'', speeds'']
  xd <- veccat casadiModule [dummyDdtCoords'', ddtSpeeds'']

  actionsParams <- veccat casadiModule [actions'', params'']
  em <- veccat casadiModule []

  daeIn <- callMethodHs cm "daeIn" [x, em, actionsParams, em, xd] noKwParms
  daeOut <- callMethodHs cm "daeOut" [sysErr] noKwParms

  fmap DAE $ sxFunction casadiModule daeIn daeOut




writeDae :: String -> System -> IO String
writeDae funname cs = do
  let coords' = HS.toList (csCoords cs)
      coords = map toExpr coords'
      
      speeds' = HS.toList (csSpeeds cs)
      ddtSpeeds = map (toExpr . ddt) speeds'
      speeds = map toExpr speeds'

      actions = map toExpr $ HS.toList (csActions cs)
      params  = map toExpr $ HS.toList (csParams cs)
      odeError = map (toExpr . implicitError) (kanes cs) -- ++ (zipWith (-)

      ddtCoords = map f coords'
        where
          f c = case HM.lookup c (csCoordDerivs cs) of
            Nothing -> error $ "need to set derivative of " ++ show c
            Just d -> toExpr d

      symString :: Sym -> String
      symString (Sym name) = name
      symString (SymDependent name 0 _) = name
      symString (SymDependent name 1 _) = "ddt__" ++ name
      symString (SymDependent name k _) = "d" ++ show k ++ "_dt" ++ show k ++ "___" ++ name

      mangle = (++  "___mangled_lol")

      ssym (ESym s) = "ssym(\"" ++ symString s ++ "\")"
      ssym s = error "ssym got non ESym: " ++ show s

      ssymD (ESym s) = "ssym(\"" ++ mangle (symString s) ++ "\")"
      ssymD s = error "ssymD got non ESym: " ++ show s

      showlist xs = '[':intercalate "," xs ++ "]"


  fg <- toFunGraph
        (coords :* speeds :* ddtSpeeds :* actions :* params)
        (odeError :* ddtCoords)

  return $ unlines $
    [ "from casadi import *"
    , "from casadi.tools import *"
--    , "from pylab import *"
    , "from numpy import *"
    , ""
    , showPy funname fg
    , "coords  = veccat( " ++ showlist (map ssym coords) ++ " )"
    , "speeds  = veccat( " ++ showlist (map ssym speeds) ++ " )"
    , "dcoords  = veccat( " ++ showlist (map (ssymD . toExpr . ddt) coords') ++ " )"
    , "dspeeds  = veccat( " ++ showlist (map ssymD ddtSpeeds) ++ " )"
    , "actions = veccat( " ++ showlist (map ssym actions) ++ " )"
    , "params  = veccat( " ++ showlist (map ssym params) ++ " )"
    , ""
    , "[stateError_, coordDerivs_] = " ++ funname ++"(coords, speeds, dspeeds, actions, params)"
    , "stateError = veccat( stateError_ )"
    , "coordDerivs = veccat( coordDerivs_ )"
    , "sys = veccat( [stateError, dcoords-coordDerivs] )"
    , ""
    , "states  = veccat( [ coords,  speeds] )"
    , "dstates = veccat( [dcoords, dspeeds] )"
    , ""
    , "f = SXFunction("
    , "  {'NUM': CONTROL_DAE_NUM_IN, "
    , "    CONTROL_DAE_X: states,"
    , "    CONTROL_DAE_P: params,"
    , "    CONTROL_DAE_XDOT: dstates,"
    , "    CONTROL_DAE_U: actions}, [sys])"
    , ""
    , "f.init()"
    ]

someSys :: System
someSys = getSystem $ do
  n <- newtonianBases

  q <- addCoord "q"
  r <- addCoord "r"
  derivIsSpeed q
  derivIsSpeed r

  mass <- addParam "m"
  g <- addParam "g"
  tension <- addAction "T"

  b <- rotY n q "B"

  let r_b_n0 = CC.relativePoint N0 (CC.zVec r b)

  basket <- addParticle mass r_b_n0
  addForceAtCm basket (CC.zVec (mass*g) n)
  addForceAtCm basket (CC.zVec (-tension) b)

run :: IO ()
run = writeDae "foo" someSys >>= putStrLn
