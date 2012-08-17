{-# OPTIONS_GHC -Wall #-}

module Classy.Casadi.Integrator where

import Control.Applicative ( (<$>) )
import Python.Exceptions
import Python.Interpreter
import Python.Objects
import Foreign.C.Types ( CDouble )

import qualified Classy.Convenience as CC
import Classy.State hiding ( run )
import Classy.Types
import Classy.Casadi.DAE
import Classy.Casadi.Bindings

newtype Integrator = Integrator PyObject
instance ToPyObject Integrator where toPyObject (Integrator p) = return p
instance FromPyObject Integrator where fromPyObject = return . Integrator

run :: IO ()
run = do
  casadiModule <- casadiInit
  dae <- mkDae casadiModule someSys
  int <- mkIdasIntegrator casadiModule dae

  setOption int "fsens_err_con" True
  setOption int "quad_err_con" True
  setOption int "abstol" (1e-12::CDouble)
  setOption int "reltol" (1e-12::CDouble)
  setOption int "fsens_abstol" (1e-6::CDouble)
  setOption int "fsens_reltol" (1e-6::CDouble)
  setOption int "asens_abstol" (1e-6::CDouble)
  setOption int "asens_reltol" (1e-6::CDouble)
--  setOption int "exact_jacobian" exact_jacobian
--  setOption int "finite_difference_fsens" finite_difference_fsens
  setOption int "max_num_steps" (100000 :: Integer)

  setOption int "t0" (0::Integer)
  setOption int "tf" (5::Integer)
--  showPyObject integrator >>= putStrLn

  -- Set parameters
  setParams casadiModule int [10,1,9.8]
-- 
  -- Set inital state
  setX0 casadiModule int [10, 0.5, 0, 0.1]
-- 
-- # Integrate
  ret <- evaluate int
  showPyObject ret >>= putStrLn

evaluate :: Integrator -> IO PyObject
evaluate (Integrator int) =
  handlePy (\x -> ("evaluate: " ++) . show <$> formatException x >>= error) $ do
    runMethodHs int "evaluate" noParms noKwParms
    callMethodHs int "output" noParms noKwParms

setX0 :: CasadiModule -> Integrator -> [CDouble] -> IO ()
setX0 (CasadiModule cm) (Integrator int) x0s =
  handlePy (\x -> ("setX0: " ++) . show <$> formatException x >>= error) $ do
    d <- getattr cm "INTEGRATOR_X0"
    x0s' <- toPyObject x0s
    runMethodHs int "setInput" [x0s', d] noKwParms

setParams :: CasadiModule -> Integrator -> [CDouble] -> IO ()
setParams (CasadiModule cm) (Integrator int) ps =
  handlePy (\x -> ("setParams: " ++) . show <$> formatException x >>= error) $ do
    d <- getattr cm "INTEGRATOR_P"
    ps' <- toPyObject ps
    runMethodHs int "setInput" [ps', d] noKwParms

mkIdasIntegrator :: CasadiModule -> DAE -> IO Integrator
mkIdasIntegrator (CasadiModule casadiModule) dae =
  handlePy (\x -> ("mkIdasIntegrator: " ++) . show <$> formatException x >>= error) $ do
  int@(Integrator i) <- callMethodHs casadiModule "IdasIntegrator" [dae] noKwParms
  runMethodHs i "init" noParms noKwParms
  return int

setOption :: ToPyObject a => Integrator -> String -> a -> IO ()
setOption (Integrator i) option value =
  handlePy (\x -> ("setOption: " ++) . show <$> formatException x >>= error) $ do
    opt <- toPyObject option
    val <- toPyObject value
    runMethodHs i "setOption" [opt, val] noKwParms

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
