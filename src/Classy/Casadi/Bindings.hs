{-# OPTIONS_GHC -Wall #-}

module Classy.Casadi.Bindings( SXM(..)
                             , SXFunction(..)
                             , CasadiModule(..)
                             , casadiInit
                             , sym
                             , vsym
                             , msym
                             , sxInt
                             , sxDouble
                             , gradient
                             , jacobian
                             , hessian
                             , matrixMultiply
                             , transpose
                             , veccat
                             , horzcat
                             , vertcat
                             , sxFunction
                             , generateCode
                               -- * binary
                             , sxmMul
                             , sxmDiv
                             , sxmAdd
                             , sxmSub
                             , sxmPow
                               -- * unary
                             , sxmSignum
                             , sxmNeg
                             , sxmExp
                             , sxmAbs
                             , sxmLog
                             , sxmAcos
                             , sxmAsin
                             , sxmAtan
                             , sxmCos
                             , sxmCosh
                             , sxmSin
                             , sxmSinh
                             , sxmTan
                             , sxmTanh
                             , sxmSqrt
                             , pyShow
                             ) where

import Control.Applicative ( (<$>) )
import Foreign.C.Types ( CDouble, CInt )
import Python.Exceptions
import Python.Interpreter
import Python.Objects 
import System.IO.Unsafe ( unsafePerformIO )

newtype SXM = SXM PyObject
newtype SXFunction = SXFunction PyObject

newtype CasadiModule = CasadiModule PyObject

instance Show SXM where
--  show (SXM x) = show x
  show = unsafePerformIO . pyShow
  
instance Show SXFunction where
--  show (SXFunction x) = show x
  show = unsafePerformIO . pyShow

instance FromPyObject SXM where fromPyObject = return . SXM
instance ToPyObject SXM where toPyObject (SXM x) = return x

instance FromPyObject CasadiModule where fromPyObject = return . CasadiModule
instance ToPyObject CasadiModule where toPyObject (CasadiModule x) = return x

instance ToPyObject SXFunction where toPyObject (SXFunction p) = return p
instance FromPyObject SXFunction where fromPyObject = return . SXFunction

-- some orphan instances
instance ToPyObject Bool where
  toPyObject True = pyRun_String "True" Py_eval_input []
  toPyObject False = pyRun_String "False" Py_eval_input []

instance ToPyObject PyObject where
  toPyObject = return
instance FromPyObject PyObject where
  fromPyObject = return

pyShow :: ToPyObject a => a -> IO String
pyShow x = callByNameHs "str" [x] noKwParms

-------------------------------------------------------------
sym :: CasadiModule -> String -> IO SXM
sym casadi name = msym casadi name (1,1)
  
vsym :: CasadiModule -> String -> Int -> IO SXM
vsym casadi name n = msym casadi name (n,1)
  
msym :: CasadiModule -> String -> (Int,Int) -> IO SXM
--msym casadi name (r,c) = callMethodHs casadi "ssym" [toPyObject name, toPyObject r, toPyObject c] noKwParms
msym (CasadiModule casadi) name (r,c) = do
  ssym <- getattr casadi "ssym"
  name' <- toPyObject name
  r' <- toPyObject (toInteger r)
  c' <- toPyObject (toInteger c)
  mat <- pyObject_Call ssym [name', r', c'] []
  return (SXM mat)
  
sxInt :: CasadiModule -> Int -> IO SXM
sxInt (CasadiModule casadi) k 
  | withinCIntBounds k = callMethodHs casadi "ssym" [(fromIntegral k)::CInt] noKwParms
  | otherwise = error $ "sxInt got out of range value: "++show k++", range: "++show (minCInt,maxCInt)
        where
            withinCIntBounds x = and [fromIntegral x <= maxCInt, fromIntegral x >= minCInt]
            maxCInt = toInteger (maxBound :: CInt)
            minCInt = toInteger (minBound :: CInt)

sxDouble :: CasadiModule -> Double -> IO SXM
sxDouble (CasadiModule casadi) x = callMethodHs casadi "ssym" [(realToFrac x)::CDouble] noKwParms

--------------------------------------------------------------
gradient :: CasadiModule -> SXM -> SXM -> IO SXM
gradient (CasadiModule casadi) ex args = callMethodHs casadi "gradient" [ex, args] noKwParms
  
jacobian :: CasadiModule -> SXM -> SXM -> IO SXM
jacobian (CasadiModule casadi) ex args = callMethodHs casadi "jacobian" [ex, args] noKwParms

hessian :: CasadiModule -> SXM -> SXM -> IO SXM
hessian (CasadiModule casadi) ex args = callMethodHs casadi "hessian" [ex, args] noKwParms

---------------------------------------------------------------

sxFunction :: CasadiModule -> [SXM] -> [SXM] -> IO SXFunction
sxFunction (CasadiModule casadi) xs zs =
  handlePy (\x -> ("sxFunction: " ++) . show <$> formatException x >>= error) $ do
    f@(SXFunction fun) <- callMethodHs casadi "SXFunction" [xs, zs] noKwParms
    runMethodHs fun "init" noParms noKwParms
    return f

generateCode :: SXFunction -> String -> IO String
generateCode (SXFunction fun) filename = do
  callMethodHs fun "generateCode" [filename] noKwParms

-------------------------------------------------------------
  
callSXMatrix :: String -> CasadiModule -> [SXM] -> IO SXM
callSXMatrix method (CasadiModule casadi) xs =
  handlePy (\x -> ("callSXMatrix: " ++) . show <$> formatException x >>= error) $ do
    sxmatrix <- getattr casadi "SXMatrix"
    callMethodHs sxmatrix method xs noKwParms


binarySXMatrix :: String -> CasadiModule -> SXM -> SXM -> IO SXM
binarySXMatrix method casadi x y = callSXMatrix method casadi [x,y]

unarySXMatrix :: String -> CasadiModule -> SXM -> IO SXM
unarySXMatrix method casadi x = callSXMatrix method casadi [x]

matrixMultiply :: CasadiModule -> SXM -> SXM -> IO SXM
matrixMultiply = binarySXMatrix "mul"

sxmMul :: CasadiModule -> SXM -> SXM -> IO SXM
sxmMul = binarySXMatrix "__mul__"

sxmDiv :: CasadiModule -> SXM -> SXM -> IO SXM
sxmDiv = binarySXMatrix "__div__"

sxmAdd :: CasadiModule -> SXM -> SXM -> IO SXM
sxmAdd = binarySXMatrix "__add__"

sxmSub :: CasadiModule -> SXM -> SXM -> IO SXM
sxmSub = binarySXMatrix "__sub__"

sxmPow :: CasadiModule -> SXM -> SXM -> IO SXM
sxmPow = binarySXMatrix "__pow__"
-----------------------------------

transpose :: CasadiModule -> SXM -> IO SXM
transpose = unarySXMatrix "T"

sxmSignum :: CasadiModule -> SXM -> IO SXM
sxmSignum = unarySXMatrix "__sign__"

sxmNeg :: CasadiModule -> SXM -> IO SXM
sxmNeg = unarySXMatrix "__neg__"

sxmExp :: CasadiModule -> SXM -> IO SXM
sxmExp = unarySXMatrix "exp"

sxmAbs :: CasadiModule -> SXM -> IO SXM
sxmAbs = unarySXMatrix "fabs"

sxmLog :: CasadiModule -> SXM -> IO SXM
sxmLog = unarySXMatrix "log"

sxmAcos :: CasadiModule -> SXM -> IO SXM
sxmAcos = unarySXMatrix "arccos"

sxmAsin :: CasadiModule -> SXM -> IO SXM
sxmAsin = unarySXMatrix "arcsin"

sxmAtan :: CasadiModule -> SXM -> IO SXM
sxmAtan = unarySXMatrix "arctan"

sxmCos :: CasadiModule -> SXM -> IO SXM
sxmCos = unarySXMatrix "cos"

sxmCosh :: CasadiModule -> SXM -> IO SXM
sxmCosh = unarySXMatrix "cosh"

sxmSin :: CasadiModule -> SXM -> IO SXM
sxmSin = unarySXMatrix "sin"

sxmSinh :: CasadiModule -> SXM -> IO SXM
sxmSinh = unarySXMatrix "sinh"

sxmTan :: CasadiModule -> SXM -> IO SXM
sxmTan = unarySXMatrix "tan"

sxmTanh :: CasadiModule -> SXM -> IO SXM
sxmTanh = unarySXMatrix "tanh"

sxmSqrt :: CasadiModule -> SXM -> IO SXM
sxmSqrt = unarySXMatrix "sqrt"

----------------------------------

veccat :: CasadiModule -> [SXM] -> IO SXM
veccat (CasadiModule cm) ins =
  handlePy (\x -> ("veccat: " ++) . show <$> formatException x >>= error) $
  callMethodHs cm "veccat" [ins] noKwParms

horzcat :: CasadiModule -> [SXM] -> IO SXM
horzcat (CasadiModule cm) ins =
  handlePy (\x -> ("horzcat: " ++) . show <$> formatException x >>= error) $
  callMethodHs cm "horzcat" [ins] noKwParms

vertcat :: CasadiModule -> [SXM] -> IO SXM
vertcat (CasadiModule cm) ins =
  handlePy (\x -> ("vertcat: " ++) . show <$> formatException x >>= error) $
  callMethodHs cm "vertcat" [ins] noKwParms

---------------------------------

casadiInit :: IO CasadiModule
casadiInit = do
  py_initialize
  fmap CasadiModule (pyImport_ImportModule "casadi")

--main :: IO ()
--main = do
--  casadi <- casadiInit
----  _ <- pyRun_SimpleString "from casadi import *"
----  print casadi
----  d <- pyModule_GetDict casadi
--  
----  b <- pyRun_SimpleString "casadi.ssym('t')"
----  print b
--  x <- vsym casadi "x" 5
--  y <- sym casadi "y"
--  
--  z <- mul casadi x y
--  z2 <- mul casadi x z
--  
--  fun <- sxFunction casadi [x,y] [z,z2]
--  
----  print z
----  print fun
--  
--  dz2dx <- hessian casadi z2 x
--  print dz2dx
--
----  z' <- sxShowPyObject z
----  z'' <- sxReprOf z
----  putStrLn z'
----  putStrLn z''
