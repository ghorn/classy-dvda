{-# OPTIONS_GHC -Wall #-}

module Classy.Casadi where

import Control.Monad.State.Lazy
import Data.List ( intercalate )
import qualified Data.HashMap.Lazy as HM
import qualified Data.HashSet as HS
import Text.Printf ( printf )

import Dvda.Codegen.PythonGen
import Dvda.Codegen.WriteFile
import Dvda.FunGraph ( (:*)(..), toFunGraph )
import Dvda.Expr ( Expr(..), Sym(..) )

import qualified Classy.Convenience as CC
import Classy.Differentiation ( ddt )
import Classy.State hiding ( run )
import Classy.Types


toExpr :: Sca -> Expr Double
toExpr (SExpr e _) = e
toExpr (SNeg e) = negate (toExpr e)
toExpr (SAdd x y) = toExpr x + toExpr y
toExpr (SSub x y) = toExpr x - toExpr y
toExpr (SMul x y) = toExpr x * toExpr y
toExpr (SDiv x y) = toExpr x / toExpr y
toExpr e@(SDot _ _) = error $ "toExpr got something it couldn't convert to an Expr: " ++ show e

showlist :: [String] -> String
showlist xs = '[' : intercalate ", " xs ++ "]"

someSys :: ClassyState
someSys = flip execState emptyClassyState $ do
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
  addForce basket (CC.zVec (mass*g) n)
  addForce basket (CC.zVec (-tension) b)

run :: IO ()
run = writeIdasIntegrator "blah" someSys

ssym :: Expr Double -> String
ssym (ESym (Sym name)) = "ssym(\"" ++ name ++ "\")"
ssym (ESym (SymDependent name 0 _)) = "ssym(\"" ++ name ++ "\")"
ssym (ESym (SymDependent name 1 _)) = "ssym(\"ddt__" ++ name ++ "\")"
ssym (ESym (SymDependent name k _)) = printf "ssym(\"d%d_dt%d___%s\")" k k name
ssym _ = error "why the heck would you pass a non-ESym to ssym?"

ssymD :: Expr Double -> String
ssymD (ESym (Sym name)) = "ssym(\"" ++ name ++ "__\")"
ssymD (ESym (SymDependent name 0 _)) = "ssym(\"" ++ name ++ "__\")"
ssymD (ESym (SymDependent name 1 _)) = "ssym(\"ddt__" ++ name ++ "__\")"
ssymD (ESym (SymDependent name k _)) = printf "ssym(\"d%d_dt%d___%s__\")" k k name
ssymD _ = error "why the heck would you pass a non-ESym to ssym?"

implicitError :: Num a => Equation a -> a
implicitError (Equation lhs EQ rhs) = lhs - rhs
implicitError (Equation _ ord _) = error $ "implicitError only works on Eq, not " ++ show ord

writeIdasIntegrator :: String -> ClassyState -> IO ()
writeIdasIntegrator name cs = do
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

      funname = "setup_" ++ name
      pySrc =
        [ "from casadi import *"
        , "from casadi.tools import *"
        , "from setup_" ++ name ++ " import setup_" ++ name
        , "from pylab import *"
        , ""
        , "coords  = veccat( " ++ showlist (map ssym coords) ++ " )"
        , "speeds  = veccat( " ++ showlist (map ssym speeds) ++ " )"
        , "dcoords  = veccat( " ++ showlist (map (ssymD . toExpr . ddt) coords') ++ " )"
        , "dspeeds  = veccat( " ++ showlist (map ssymD ddtSpeeds) ++ " )"
        , "actions = veccat( " ++ showlist (map ssym actions) ++ " )"
        , "params  = veccat( " ++ showlist (map ssym params) ++ " )"
        , ""
        , "[stateError_, coordDerivs_] = setup_"++ name ++"(coords, speeds, dspeeds, actions, params)"
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
  
  fg <- toFunGraph (coords :* speeds :* ddtSpeeds :* actions :* params) (odeError :* ddtCoords)
  _ <- writeSourceFile ("from numpy import *\n\n"++showPy funname fg) "." (funname ++ ".py")
  _ <- writeSourceFile (unlines pySrc) "." "runpy.py"
  return ()
