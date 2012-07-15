{-# OPTIONS_GHC -Wall #-}

module Types ( Sca(..)
             , Vec(..)
             , XYZ(..)
             , Basis(..)
             , Frame(..)
             , Rotation(..)
             , zeroVec
             , removeZeros
             , cross
             , scale
             , scaleBasis
             ) where

import qualified Data.Array.Repa as Repa
import Data.Hashable
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM hiding ( fromList ) -- only use fromListWith
import Data.List ( intersperse )
import Data.Maybe ( catMaybes )
--import Debug.Trace

import Dvda hiding ( scale, vec )
import Dvda.Expr ( Expr(..), Const(..), isVal )
import Dvda.BinUn ( BinOp(..), lassoc, rassoc, showBinary )

data Sca = SExpr (Expr Z Double)
         | SNeg Sca
         | SAdd Sca Sca
         | SSub Sca Sca
         | SMul Sca Sca
         | SDiv Sca Sca
         | SZero
         | SOne deriving Eq

data Basis = Basis Frame XYZ
           | Cross Basis Basis deriving Eq

data Vec = Vec (HashMap Basis Sca) deriving Eq

data XYZ = X | Y | Z deriving Eq

data Frame = NewtonianFrame String
           | RFrame Frame Rotation String deriving Eq

data Rotation = RotSpeed Vec
              | RotCoord Vec
              | RotCoordSpeed Vec Vec deriving (Show, Eq)

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
  hash (Vec hm) = hash "Vec" `combine` hash (HM.toList hm)

instance Hashable Basis where
  hash (Basis f xyz) = hash "Basis" `combine` hash f `combine` hash xyz
  hash (Cross x y) = hash "Cross" `combine` hash x `combine` hash y

instance Hashable XYZ where
  hash X = hash "X"
  hash Y = hash "Y"
  hash Z = hash "Z"

instance Hashable Frame where
  hash (NewtonianFrame name) = hash "NewtonianFrame" `combine` hash name
  hash (RFrame frame rot name) = hash "RFrame" `combine` hash frame `combine` hash rot `combine` hash name

instance Hashable Rotation where
  hash (RotCoord q)        = hash "RotCoord" `combine` hash q
  hash (RotSpeed v)        = hash "RotSpeed" `combine` hash v
  hash (RotCoordSpeed q v) = hash "RotCoordSpeed" `combine` hash q `combine` hash v
  

------------------------- Num/Fractional instances ---------------------------------
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
  (+) (Vec x) (Vec y) = removeZeros $ Vec $ HM.unionWith (+) x y
  (-) (Vec x) (Vec y) = removeZeros $ Vec $ HM.unionWith (-) x y
  negate (Vec x) = removeZeros $ Vec $ HM.map negate x

  (*) = error "(*) not instanced for Vec"
  abs = error "abs not instanced for Vec"
  signum = error "signum not instanced for Vec"
  fromInteger 0 = zeroVec
  fromInteger _ = error "Num Vec's fromInteger only instanced for 0"

-------------------------- show instances ----------------------------------
paren :: String -> String
paren x = "("++ x ++")"

parenx :: Sca -> BinOp -> String -> String
parenx (SExpr (EBinary xop _ _)) op = if lassoc xop op then id else paren
parenx (SMul _ _) op =                if lassoc Mul op then id else paren
parenx (SDiv _ _) op =                if lassoc Div op then id else paren
parenx (SAdd _ _) op =                if lassoc Add op then id else paren
parenx (SSub _ _) op =                if lassoc Sub op then id else paren
parenx _ _ = id

pareny :: Sca -> BinOp -> String -> String
pareny (SExpr (EBinary yop _ _)) op = if rassoc op yop then id else paren
pareny (SMul _ _) op =                if rassoc op Mul then id else paren
pareny (SDiv _ _) op =                if rassoc op Div then id else paren
pareny (SAdd _ _) op =                if rassoc op Add then id else paren
pareny (SSub _ _) op =                if rassoc op Sub then id else paren
pareny _ _ = id

instance Show Sca where
  show (SExpr x) = show x
  show (SMul x y) = parenx x Mul (show x) ++ " " ++ showBinary Mul ++ " " ++ pareny y Mul (show y)
  show (SDiv x y) = parenx x Div (show x) ++ " " ++ showBinary Div ++ " " ++ pareny y Div (show y)
  show (SAdd x y) = parenx x Add (show x) ++ " " ++ showBinary Add ++ " " ++ pareny y Add (show y)
  show (SSub x y) = parenx x Sub (show x) ++ " " ++ showBinary Sub ++ " " ++ pareny y Sub (show y)
  show (SNeg x) = "(-(" ++ show x ++ "))"
  show (SZero) = "0"
  show (SOne) = "1"

instance Show Vec where
  show (Vec hm) = concat $ intersperse " + " (map show' (HM.toList hm))
    where
      show' (b, sca@(SAdd _ _)) = "(" ++ show sca ++ ")*" ++ show b
      show' (b, sca@(SSub _ _)) = "(" ++ show sca ++ ")*" ++ show b
      show' (b, sca)
        | isVal' 1 sca = show b
        | otherwise   = show sca ++ "*" ++ show b

instance Show Basis where
  show (Basis f xyz) = show f ++ show xyz
  show (Cross b0 b1) = "( " ++ show b0 ++ " x " ++ show b1 ++ " )"

instance Show XYZ where
  show X = "x>"
  show Y = "y>"
  show Z = "z>"

instance Show Frame where
  show (NewtonianFrame n) = n
  show (RFrame _ _ n) = n

------------------------------ utilities -------------------------------------

-- | if (a x b) is zero, return Nothing
--   .
--   if (a x b) is non-zero, return (basis0 x basis1, sign*scalar0*scalar1)
crossBases :: (Basis, Sca) -> (Basis, Sca) -> Maybe (Basis, Sca)
crossBases (b0@(Basis f0 xyz0), s0) (b1@(Basis f1 xyz1), s1)
  | f0 == f1 = case (xyz0, xyz1) of
    (X,Y) -> Just (Basis f0 Z, s0*s1)
    (Y,Z) -> Just (Basis f0 X, s0*s1)
    (Z,X) -> Just (Basis f0 Y, s0*s1)
    (Z,Y) -> Just (Basis f0 X, -(s0*s1))
    (Y,X) -> Just (Basis f0 Z, -(s0*s1))
    (X,Z) -> Just (Basis f0 Y, -(s0*s1))
    (X,X) -> Nothing
    (Y,Y) -> Nothing
    (Z,Z) -> Nothing
  | otherwise = Just (Cross b0 b1, s0*s1)
crossBases (b0,s0) (b1,s1) = Just (Cross b0 b1, s0*s1)

cross :: Vec -> Vec -> Vec
cross (Vec hm0) (Vec hm1) =
  removeZeros $ Vec $ HM.fromListWith (+) $
  catMaybes [crossBases (b0,x0) (b1,x1) | (b0,x0) <- HM.toList hm0, (b1,x1) <- HM.toList hm1]

isVal' :: Double -> Sca -> Bool
isVal' x (SExpr e) = isVal x e
isVal' 0 SZero = True
isVal' 1 SOne = True
isVal' x (SNeg s) = isVal' (-x) s
isVal' _ _ = False

scale :: Sca -> Vec -> Vec
scale s vec@(Vec hm)
  | isVal' 0 s = zeroVec
  | isVal' 1 s = vec
  | isVal' (-1) s = Vec $ HM.map negate hm
  | otherwise = removeZeros $ Vec $ HM.map (s *) hm

zeroVec :: Vec
zeroVec = Vec HM.empty

removeZeros :: Vec -> Vec
--removeZeros (Vec hm) = trace ("\n-------------\nbefore: "++show hm ++ "\nafter:  "++show ret) $ Vec ret
removeZeros (Vec hm) = Vec ret
  where
    ret = HM.filter (not . (isVal' 0)) hm

scaleBasis :: Sca -> Basis -> Vec
scaleBasis s b = removeZeros $ Vec (HM.singleton b s)
