{-# OPTIONS_GHC -Wall #-}

module Types ( Sca(..)
             , Vec(..)
             , XYZ(..)
             , Basis(..)
             , Frame(..)
             , Rotation(..)
             , Dyad(..)
             , Dyadic(..)
             , Equation(..)
             , Equations(..)
             , zeroVec
             , removeZeros
             , isVal
             , equivBases
             ) where

import Data.Hashable
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM hiding ( fromList ) -- only use fromListWith
import Data.HashSet( HashSet )
import qualified Data.HashSet as HS
import Data.List ( intercalate )

import Dvda hiding ( Z(..) )
import qualified Dvda as Dvda
import qualified Dvda.Expr as Expr
import Dvda.Expr ( Expr(..), Const(..) )
import Dvda.BinUn ( BinOp(..), lassoc, rassoc, showBinary )

data Sca = SExpr (Expr Dvda.Z Double) (Maybe Int)
         | SDot (Basis,Basis) Sca
         | SNeg Sca
         | SAdd Sca Sca
         | SSub Sca Sca
         | SMul Sca Sca
         | SDiv Sca Sca

instance Eq Sca where
  (==) (SExpr x mx) (SExpr y my) = x == y && mx == my
  (==) (SDot (bx0, bx1) x) (SDot (by0, by1) y) = x == y && or [bx0 == by0 && bx1 == by1, bx0 == by1 && bx1 == by0]
  (==) (SNeg x) (SNeg y) = x == y
  (==) (SAdd x0 x1) (SAdd y0 y1) = x0 == y0 && x1 == y1
  (==) (SSub x0 x1) (SSub y0 y1) = x0 == y0 && x1 == y1
  (==) (SMul x0 x1) (SMul y0 y1) = x0 == y0 && x1 == y1
  (==) (SDiv x0 x1) (SDiv y0 y1) = x0 == y0 && x1 == y1
  (==) _ _ = False
  

data Basis = Basis Frame XYZ
           | Cross Basis Basis deriving Eq

data Vec = Vec (HashMap Basis Sca) deriving Eq

data XYZ = X | Y | Z deriving Eq

data Frame = NewtonianFrame String
           | RFrame Frame Rotation String deriving Eq

data Rotation = RotSpeed (Sca,Sca,Sca)
              | RotCoord Vec deriving (Show, Eq)
--              | RotCoordSpeed Vec Vec

data Dyad = Dyad Sca Basis Basis
instance Show Dyad where
  show (Dyad x b1 b2) = "(" ++ show x ++ ")*" ++ show b1 ++ show b2

data Dyadic = Dyadic ((Dyad, Dyad, Dyad), (Dyad, Dyad, Dyad), (Dyad, Dyad, Dyad))
instance Show Dyadic where
  show (Dyadic ((xx,xy,xz), (yx,yy,yz), (zx,zy,zz))) = intercalate " + " $ map show xs
    where
      xs = filter (\(Dyad s _ _) -> s /= 0) [xx,xy,xz,yx,yy,yz,zx,zy,zz]

data Equation a = Equation a Ordering a
instance Show a => Show (Equation a) where
  show (Equation lhs EQ rhs) = show lhs ++ " == " ++ show rhs
  show (Equation lhs LT rhs) = show lhs ++ " < " ++ show rhs
  show (Equation lhs GT rhs) = show lhs ++ " > " ++ show rhs

data Equations a = Equations [Equation a]
instance Show a => Show (Equations a) where
  show (Equations eqs) = unlines (map show eqs)

-- | Lots of things carry around a list of all equivalent bases
--   For example: if you start with frame N and rotate about Nz to get A, then Nz == Az
type EquivBases = (HashSet (Basis, Basis))

------------------------- get equivilant bases ---------------------------
class HasEquivBases a where
  equivBases :: a -> EquivBases

instance HasEquivBases Frame where
  equivBases (NewtonianFrame _) = HS.empty
  equivBases (RFrame _ (RotSpeed _) _) = HS.empty
  equivBases f@(RFrame f0 (RotCoord rotVec) _) = case rotBases of
    [Basis frame xyz] -> if frame == f0
                         then HS.insert (Basis f0 xyz, Basis f xyz) (equivBases f0)
                         else HS.empty
    _ -> HS.empty
    where
      rotBases = HM.keys $ (\(Vec hm) -> hm) rotVec

instance HasEquivBases Basis where
  equivBases (Basis frame _) = equivBases frame
  equivBases (Cross b0 b1) = HS.union (equivBases b0) (equivBases b1)


-------------------------- hashable instances ------------------------------------
instance Hashable Sca where
  hash (SExpr x (Just mx))  = hash "SExpr" `combine` hash x `combine` mx
  hash (SExpr x _)  = hash "SExpr" `combine` hash x
  -- sort the hashes because (x `dot` y) == (y `dot` x), as defined in Eq instance
  hash (SDot (b0,b1) x) = hash "SExpr" `combine` hash x `combine` min hb0 hb1 `combine` max hb0 hb1
    where
      hb0 = hash b0
      hb1 = hash b1
  hash (SNeg x)   = hash "SNeg" `combine` hash x
  hash (SAdd x y) = hash "SAdd" `combine` hash x `combine` hash y
  hash (SSub x y) = hash "SSub" `combine` hash x `combine` hash y
  hash (SMul x y) = hash "SMul" `combine` hash x `combine` hash y
  hash (SDiv x y) = hash "SDiv" `combine` hash x `combine` hash y

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
--  hash (RotCoordSpeed q v) = hash "RotCoordSpeed" `combine` hash q `combine` hash v
  

------------------------- Num/Fractional instances ---------------------------------
instance Num Sca where
  (*) x y
    | isVal 0 x || isVal 0 y = 0
    | isVal 1 x = y
    | isVal 1 y = x
    | otherwise = case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' * y') Nothing
                                _ -> SMul x y

  (+) x y
    | x == -y || -x == y = 0
    | isVal 0 x = y
    | isVal 0 y = x
    | otherwise = case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' + y') Nothing
                                _ -> SAdd x y

  (-) x y
    | x == y = 0
    | isVal 0 x = -y
    | isVal 0 y = x
    | otherwise = case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' - y') Nothing
                                _ -> SSub x y

  abs = error "abs not defined for Num Sca"
  signum = error "signum not defined for Num Sca"
  fromInteger = (\x -> x Nothing) . SExpr . EConst . (CSingleton Dvda.Z) . fromInteger
  negate (SNeg x) = x
  negate x = SNeg x

instance Fractional Sca where
  (/) x y
    | isVal 1 y = x
    | isVal 0 y = error "Fractional Sca (/): divide by 0"
    | isVal 0 x = 0
    | otherwise = case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' / y') Nothing
                                _ -> SDiv x y

  fromRational = (\x -> x Nothing) . SExpr . EConst . (CSingleton Dvda.Z) . fromRational

instance Num Vec where
  (+) (Vec x) (Vec y) = removeZeros $ Vec (HM.unionWith (+) x y)
  (-) (Vec x) (Vec y) = removeZeros $ Vec (HM.unionWith (-) x y)
  negate (Vec x) = removeZeros $ Vec (HM.map negate x)

  (*) = error "(*) not instanced for Vec"
  abs = error "abs not instanced for Vec"
  signum = error "signum not instanced for Vec"
  fromInteger 0 = zeroVec
  fromInteger _ = error "Num Vec's fromInteger only instanced for 0"

-------------------------- show instances ----------------------------------
paren :: String -> String
paren x = "("++ x ++")"

parenx :: Sca -> BinOp -> String -> String
parenx (SExpr (EBinary xop _ _) _) op = if lassoc xop op then id else paren
parenx (SMul _ _) op =                  if lassoc Mul op then id else paren
parenx (SDiv _ _) op =                  if lassoc Div op then id else paren
parenx (SAdd _ _) op =                  if lassoc Add op then id else paren
parenx (SSub _ _) op =                  if lassoc Sub op then id else paren
parenx _ _ = id

pareny :: Sca -> BinOp -> String -> String
pareny (SExpr (EBinary yop _ _) _) op = if rassoc op yop then id else paren
pareny (SMul _ _) op =                  if rassoc op Mul then id else paren
pareny (SDiv _ _) op =                  if rassoc op Div then id else paren
pareny (SAdd _ _) op =                  if rassoc op Add then id else paren
pareny (SSub _ _) op =                  if rassoc op Sub then id else paren
pareny _ _ = id

instance Show Sca where
  show (SExpr x _) = show x
  show (SDot (b0,b1) x)
     | isVal 1 x = "( " ++ show b0 ++ " . " ++ show b1 ++ " )"
     | otherwise = "( " ++ show x ++ " ) * ( " ++ show b0 ++ " . " ++ show b1 ++ " )"
  show (SMul x y) = parenx x Mul (show x) ++ " " ++ showBinary Mul ++ " " ++ pareny y Mul (show y)
  show (SDiv x y) = parenx x Div (show x) ++ " " ++ showBinary Div ++ " " ++ pareny y Div (show y)
  show (SAdd x y) = parenx x Add (show x) ++ " " ++ showBinary Add ++ " " ++ pareny y Add (show y)
  show (SSub x y) = parenx x Sub (show x) ++ " " ++ showBinary Sub ++ " " ++ pareny y Sub (show y)
  show (SNeg x) = "(-(" ++ show x ++ "))"

instance Show Vec where
  show (Vec hm)
    | HM.null hm = "0>"
    | otherwise = intercalate " + " (map show' (HM.toList hm))
    where
      show' (b, sca@(SAdd _ _)) = "( " ++ show sca ++ " ) * " ++ show b
      show' (b, sca@(SSub _ _)) = "( " ++ show sca ++ " ) * " ++ show b
      show' (b, sca)
        | isVal 1 sca = show b
        | otherwise   = show sca ++ " * " ++ show b

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


-------------------- utils ---------------
isVal :: Double -> Sca -> Bool
isVal x (SExpr e _) = Expr.isVal x e
isVal _ _ = False

zeroVec :: Vec
zeroVec = Vec HM.empty

removeZeros :: Vec -> Vec
removeZeros (Vec hm) = Vec $ HM.filter (not . (isVal 0)) hm
--removeZeros (Vec hm) = trace ("\n-------------\nbefore: "++show hm ++ "\nafter:  "++show ret) $ Vec ret
--  where
--    ret = HM.filter (not . (isVal 0)) hm