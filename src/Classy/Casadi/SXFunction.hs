{-# OPTIONS_GHC -Wall #-}

module Classy.Casadi.SXFunction ( mkSXFunction
                                , mkSXMs
                                ) where

import Control.Monad ( foldM )
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IM
import Dvda.HashMap ( HashMap )
import qualified Dvda.HashMap as HM

import Dvda.Expr ( GExpr(..), Floatings(..), Nums(..), Fractionals(..), Sym(..) )
import Dvda.FunGraph ( FunGraph, MVS(..), topSort, fgInputs, fgOutputs, fgLookupGExpr )

import Classy.Casadi.Bindings

-- | Turns a FunGraph into SXM inputs and outputs
mkSXMs :: CasadiModule -> FunGraph Double -> IO ([SXM], [SXM])
mkSXMs casadiModule fg = do
  (sxMap,inputMap) <-
    let f im k = case fgLookupGExpr fg k of
          Nothing -> error $ "mkSXFunction: couldn't find node " ++ show k ++ " in fungraph :("
          Just gexpr -> pyAssignment casadiModule im k gexpr
    in foldM f (IM.empty,HM.empty) (reverse $ topSort fg)
  let g (GSym s) = lookupErr "mkSXFunction inputIndices" (flip HM.lookup inputMap) s
      g gexpr = error $ "got non GSym input " ++ show gexpr
      inputIndices = map (fmap g) (fgInputs fg)
      outputIndices = fgOutputs fg

      inputSXMs, outputSXMs :: [MVS SXM]
      inputSXMs  = map (fmap (lookupErr "mkSXFunction inputSXMs"  (flip IM.lookup sxMap)))
                   inputIndices
      outputSXMs = map (fmap (lookupErr "mkSXFunction outputSXMs" (flip IM.lookup sxMap)))
                   outputIndices

      toSXMatrix :: MVS SXM -> IO SXM
      toSXMatrix (Sca x) = return x
      toSXMatrix (Vec xs) = veccat casadiModule xs
      toSXMatrix (Mat xs) = mapM (horzcat casadiModule) xs
                            >>= vertcat casadiModule

  inputSXM  <- mapM toSXMatrix inputSXMs
  outputSXM <- mapM toSXMatrix outputSXMs
  return (inputSXM, outputSXM)

-- | Turns a FunGraph into an SXFunction
mkSXFunction :: CasadiModule -> FunGraph Double -> IO SXFunction
mkSXFunction casadiModule fg = do
  (inputSXM, outputSXM) <- mkSXMs casadiModule fg
  sxFunction casadiModule inputSXM outputSXM

lookupErr :: Show a => String -> (a -> Maybe b) -> a -> b
lookupErr name lu x = case lu x of
  Just x' -> x'
  Nothing -> error $ name ++ ": lookup failed on node " ++ show x

symString :: Sym -> String
symString (Sym name) = name
symString (SymDependent name 0 _) = name
symString (SymDependent name 1 _) = "ddt__" ++ name
symString (SymDependent name k _) = "d" ++ show k ++ "_dt" ++ show k ++ "___" ++ name

pyAssignment :: CasadiModule -> (IntMap SXM, HashMap Sym Int) -> Int -> GExpr Double Int -> IO (IntMap SXM, HashMap Sym Int)
pyAssignment casadiModule (im,hm) k gexpr = toPyOp gexpr
  where
    luGExpr = lookupErr "pyAssignment" (flip IM.lookup im)

    bin x y op = do
      ret <- op casadiModule (luGExpr x) (luGExpr y)
      return (IM.insert k ret im,hm)
    
    un x op = do
      ret <- op casadiModule (luGExpr x)
      return (IM.insert k ret im,hm)

    toPyOp (GSym s) = do
      ret <- sym casadiModule (symString s)
      return (IM.insert k ret im, HM.insert s k hm)
    toPyOp (GConst c) = do
      ret <- sxDouble casadiModule c
      return (IM.insert k ret im,hm)
    toPyOp (GNum (FromInteger x))         = toPyOp (GConst (fromInteger x))
    toPyOp (GFractional (FromRational x)) = toPyOp (GConst (fromRational x))
    toPyOp (GNum (Mul x y))               = bin x y sxmMul
    toPyOp (GNum (Add x y))               = bin x y sxmAdd
    toPyOp (GNum (Sub x y))               = bin x y sxmSub
    toPyOp (GNum (Negate x))              = un x sxmNeg
    toPyOp (GNum (Abs x))                 = un x sxmAbs
    toPyOp (GNum (Signum x))              = un x sxmSignum
    toPyOp (GFractional (Div x y))        = bin x y sxmDiv
    toPyOp (GFloating (Pow x y))          = bin x y sxmPow
    toPyOp (GFloating (Exp x))            = un x sxmExp
    toPyOp (GFloating (Log x))            = un x sxmLog
    toPyOp (GFloating (Sin x))            = un x sxmSin
    toPyOp (GFloating (Cos x))            = un x sxmCos
    toPyOp (GFloating (ASin x))           = un x sxmAsin
    toPyOp (GFloating (ATan x))           = un x sxmAtan
    toPyOp (GFloating (ACos x))           = un x sxmAcos
    toPyOp (GFloating (Sinh x))           = un x sxmSinh
    toPyOp (GFloating (Cosh x))           = un x sxmCosh
    toPyOp (GFloating (Tanh x))           = un x sxmTanh
    toPyOp (GFloating (LogBase _ _))      = error "casadi python doesn't support LogBase"
    toPyOp (GFloating (ASinh _))          = error "casadi python doesn't support ASinh"
    toPyOp (GFloating (ATanh _))          = error "casadi python doesn't support ATanh"
    toPyOp (GFloating (ACosh _))          = error "casadi python doesn't support ACosh"
