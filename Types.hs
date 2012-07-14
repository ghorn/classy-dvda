{-# OPTIONS_GHC -Wall #-}

module Types ( Sca(..)
             , Vec(..)
             , XYZ(..)
             , Basis(..)
             , Frame(..)
             , Rotation(..)
             , cross
             , scale
             ) where

import Data.Hashable
import qualified Data.Array.Repa as Repa

import Dvda hiding ( scale )
import Dvda.Expr ( Expr(..), Const(..), isVal )

data Sca = SExpr (Expr Z Double)
         | SNeg Sca
         | SAdd Sca Sca
         | SSub Sca Sca
         | SMul Sca Sca
         | SDiv Sca Sca
         | SZero
         | SOne deriving Eq
--         | SSpeed Speed
--         | SAccel Accel
--         | SParam String
--         | SNum Double

data Basis = Basis Frame XYZ deriving Eq

data Vec = VBasis Basis Sca -- can be more than one equivalent basis
         | VZero
         | VAdd Vec Vec
         | VSub Vec Vec
         | VNeg Vec
         | VCross Vec Vec deriving Eq

data XYZ = X | Y | Z deriving Eq

data Frame = NewtonianFrame String
           | RFrame Frame Rotation String deriving Eq

data Rotation = RotCoord Vec
              | RotSpeed Vec deriving (Show, Eq)


-------------------------- hashable instances ------------------------------------
instance Hashable Sca where
  hash (SExpr x)  = hash "SExpr" `combine` hash x
  hash (SNeg x)   = hash "SNeg" `combine` hash x
  hash (SAdd x y) = hash "SAdd" `combine` hash x `combine` hash y
  hash (SSub x y) = hash "SSub" `combine` hash x `combine` hash y
  hash (SMul x y) = hash "SMul" `combine` hash x `combine` hash y
  hash (SDiv x y) = hash "SDiv" `combine` hash x `combine` hash y
  hash SZero = hash "SZero"
  hash SOne = hash "SOne"

instance Hashable Vec where
  hash (VBasis basis x) = hash "VBasis" `combine` hash basis `combine` hash x
  hash VZero = hash "VZero"
  hash (VAdd x y) = hash "VAdd" `combine` hash x `combine` hash y
  hash (VSub x y) = hash "VSub" `combine` hash x `combine` hash y
  hash (VNeg x) = hash "VNeg" `combine` hash x
  hash (VCross x y) = hash "VCross" `combine` hash x `combine` hash y

instance Hashable Basis where
  hash (Basis f xyz) = hash "Basis" `combine` hash f `combine` hash xyz

instance Hashable XYZ where
  hash X = hash "X"
  hash Y = hash "Y"
  hash Z = hash "Z"

instance Hashable Frame where
  hash (NewtonianFrame name) = hash "NewtonianFrame" `combine` hash name
  hash (RFrame frame rot name) = hash "RFrame" `combine` hash frame `combine` hash rot `combine` hash name

instance Hashable Rotation where
  hash (RotCoord v) = hash "RotCoord" `combine` hash v
  hash (RotSpeed v) = hash "RotSpeed" `combine` hash v
  

------------------------------------------------------------
instance Num Sca where
  (*) (SExpr x) (SExpr y) = SExpr $ x * y
  SZero * _ = SZero
  _ * SZero = SZero
  SOne * y = y
  x * SOne = x
  x * y = SMul x y

  (+) (SExpr x) (SExpr y) = SExpr $ x + y
  SZero + y = y
  x + SZero = x
  x + y = SAdd x y

  (-) (SExpr x) (SExpr y) = SExpr $ x - y
  SZero - y = negate y
  x - SZero = x
  x - y = SSub x y

  abs = error "abs not defined for Num Sca"
  signum = error "signum not defined for Num Sca"
  fromInteger = SExpr . EConst . (CSingleton Repa.Z) . fromInteger

instance Fractional Sca where
  (SExpr x) / (SExpr y) = SExpr $ x / y
  _ / SZero = error "divide by SZero"
  SZero / _ = SZero
  x / SOne = x
  x / y = SDiv x y

  fromRational = SExpr . EConst . (CSingleton Repa.Z) . fromRational


instance Num Vec where
  (+) VZero y = y
  (+) x VZero = x
  (+) x y = VAdd x y
  
  (-) VZero y = negate y
  (-) x VZero = x
  (-) x (VNeg y) = x + y
  (-) x y = VSub x y

  negate VZero = VZero
  negate (VNeg x) = x
  negate x = VNeg x

  (*) = error "(*) not instanced for Vec"
  abs = error "abs not instanced for Vec"
  signum = error "signum not instanced for Vec"
  fromInteger = error "fromInteger not instanced for Vec"

---------------------------------------------------------------------------
-- | if two bases are equivalent, return Just (the simplest basis), otherwise return Nothing
sameBasis :: (Frame, XYZ) -> (Frame, XYZ) -> Maybe Frame
sameBasis = error "implement sameBasis"

cross :: Vec -> Vec -> Vec
cross VZero _ = VZero
cross _ VZero = VZero
cross x@(VBasis (Basis f0 xyz0) sca0) y@(VBasis (Basis f1 xyz1) sca1)
  | f0 == f1 = case (xyz0, xyz1) of
    (X,Y) -> VBasis (Basis f0 Z)   (sca0*sca1)
    (Y,Z) -> VBasis (Basis f0 X)   (sca0*sca1)
    (Z,X) -> VBasis (Basis f0 Y)   (sca0*sca1)
    (Z,Y) -> VBasis (Basis f0 X) (-(sca0*sca1))
    (Y,X) -> VBasis (Basis f0 Z) (-(sca0*sca1))
    (X,Z) -> VBasis (Basis f0 Y) (-(sca0*sca1))
    (X,X) -> VZero
    (Y,Y) -> VZero
    (Z,Z) -> VZero
  | otherwise = VCross x y
cross x y = VCross x y

isVal' :: Double -> Sca -> Bool
isVal' x (SExpr e) = isVal x e
isVal' _ _ = False

scale :: Sca -> Vec -> Vec
scale s v
  | isVal' 0 s = VZero
  | isVal' 1 s = v
  | isVal' (-1) s = negate v
  | otherwise = scale' v
  where
    scale' (VBasis basis y) = VBasis basis (s*y)
    scale' VZero = VZero
    scale' (VAdd x y) = (scale s x) + (scale s y)
    scale' (VSub x y) = (scale s x) - (scale s y)
    scale' (VNeg x) = negate (scale s x)
    scale' (VCross x y) = (scale s x) `cross` (scale s y)

-------------------------- show instances ----------------------------------
instance Show Sca where
  show (SExpr x) = show x
--  show (SCoord x) = show x
--  show (SSpeed x) = show x
--  show (SAccel x) = show x
--  show (SParam x) = x
--  show (SNum x) = show x
  show (SMul x y) = "( " ++ show x ++ " * " ++ show y ++ " )"
  show (SDiv x y) = "( " ++ show x ++ " / " ++ show y ++ " )"
  show (SAdd x y) = "( " ++ show x ++ " + " ++ show y ++ " )"
  show (SSub x y) = "( " ++ show x ++ " - " ++ show y ++ " )"
  show (SNeg x) = "( - " ++ show x ++ " )"
--  show (SDot _ _ ) = error "maybe do something about this?"
  show (SZero) = "0"
  show (SOne) = "1"

instance Show Vec where
  show (VBasis basis SOne) = show basis
  show (VBasis basis x) = show x ++ "*" ++ show basis
  show VZero = "0>"
  show (VAdd vx vy) = "( " ++ show vx ++ " + " ++ show vy ++ " )"
  show (VSub vx vy) = "( " ++ show vx ++ " - " ++ show vy ++ " )"
  show (VNeg v) = "( -" ++ show v ++ " )"
  show (VCross vx vy) = "( " ++ show vx ++ " x " ++ show vy ++ " )"

instance Show Basis where
  show (Basis f xyz) = show f ++ show xyz

instance Show XYZ where
  show X = "x>"
  show Y = "y>"
  show Z = "z>"

instance Show Frame where
  show (NewtonianFrame n) = n
  show (RFrame _ _ n) = n
