{-# OPTIONS_GHC -Wall #-}
{-# Language TypeFamilies #-}

module Classy.Casadi.SXM ( SXM(..)
                         , mkSXM
                         ) where

import Control.Applicative ( (<$>) )

import Python.Exceptions
import Python.Interpreter
import Python.Objects

newtype SXM = SXM PyObject
instance ToPyObject SXM where
  toPyObject (SXM p) = return p
instance FromPyObject SXM where
  fromPyObject = return . SXM

mkSXM :: String -> IO SXM
mkSXM name = do
  py_initialize
  casadi <- pyImport_ImportModule "casadi"
  handlePy (\x -> ("mkSXM: " ++) . show <$> formatException x >>= error) $ do
--    md <- pyImport_AddModule "__main__" >>= pyModule_GetDict
    callMethodHs casadi "ssym" [name] noKwParms
--    pyImport "numpy"
--    _ <- pyRun_String src Py_file_input [("main_dict",md)]

--    pyRun_String "main_dict['casadifun']" Py_eval_input [("main_dict",md)]
--  return (SXM sxfun)

run :: IO ()
run = do
  (SXM yo) <- mkSXM "x"
  boo <- showPyObject yo
  putStrLn boo
--  putStrLn dae'
--  _ <- mkDae someSys
--  return ()
--
--someSys :: System
--someSys = getSystem $ do
--  n <- newtonianBases
--
--  q <- addCoord "q"
--  r <- addCoord "r"
--  derivIsSpeed q
--  derivIsSpeed r
--
--  mass <- addParam "m"
--  g <- addParam "g"
--  tension <- addAction "T"
--
--  b <- rotY n q "B"
--
--  let r_b_n0 = CC.relativePoint N0 (CC.zVec r b)
--
--  basket <- addParticle mass r_b_n0
--  addForceAtCm basket (CC.zVec (mass*g) n)
--  addForceAtCm basket (CC.zVec (-tension) b)
