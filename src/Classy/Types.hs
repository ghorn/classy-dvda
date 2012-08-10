{-# OPTIONS_GHC -Wall #-}

module Classy.Types ( Sca(..)
                    , Vec(..)
                    , XYZ(..)
                    , Point(..)
                    , Basis(..)
                    , Bases(..)
                    , Rotation(..)
                    , Dyad(..)
                    , Dyadic(..)
                    , Equation(..)
                    , Equations(..)
                    , zeroVec
                    , removeZeros
                    , isVal
                    , equivBases
                    , foldSca
                    , vecFromN0
                    , vecsFromN0
                    , time
                    , isCoord, isSpeed, isTime
                    ) where

import Data.Hashable
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM hiding ( fromList )
import Data.HashSet( HashSet )
import qualified Data.HashSet as HS
import Data.List ( intercalate )

import qualified Dvda.Expr as Expr
import Dvda.Expr ( Expr(..), Nums(..), Sym(..), sym )

data Sca = SExpr (Expr Double) (Maybe Int)
         | SDot (Basis,Basis) Sca
         | SNeg Sca
         | SAdd Sca Sca
         | SSub Sca Sca
         | SMul Sca Sca
         | SDiv Sca Sca

data Basis = Basis Bases XYZ
           | Cross Basis Basis deriving Eq

data Point = N0
           | RelativePoint Point Vec

data Vec = Vec (HashMap Basis Sca) deriving Eq

data XYZ = X | Y | Z deriving Eq

data Bases = NewtonianBases
           | RotatedBases Bases Rotation String deriving Eq

data Rotation = RotSpeed (Sca,Sca,Sca)
              | RotCoord Vec deriving (Show, Eq)
--              | RotCoordSpeed Vec Vec

data Dyad = Dyad Sca Basis Basis
instance Show Dyad where
  showsPrec d (Dyad x b1 b2) = showParen (d > 7) $
                               showsPrec 7 x . showString "*" . showsPrec 7 b1 . showsPrec 7 b2

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

  showList [x] = showString (show x)
  showList xs = showString $ intercalate "\n\n" $ map show xs

data Equations a = Equations [Equation a]
instance Show a => Show (Equations a) where
  show (Equations eqs) = unlines (map show eqs)

instance Eq Sca where
  (==) (SExpr x mx) (SExpr y my) = x == y && mx == my
  (==) (SDot (bx0, bx1) x) (SDot (by0, by1) y) =
    x == y && ((bx0 == by0 && bx1 == by1) || (bx0 == by1 && bx1 == by0))
  (==) (SNeg x) (SNeg y) = x == y
  (==) (SAdd x0 x1) (SAdd y0 y1) = x0 == y0 && x1 == y1
  (==) (SSub x0 x1) (SSub y0 y1) = x0 == y0 && x1 == y1
  (==) (SMul x0 x1) (SMul y0 y1) = x0 == y0 && x1 == y1
  (==) (SDiv x0 x1) (SDiv y0 y1) = x0 == y0 && x1 == y1
  (==) _ _ = False
  

-- | Lots of things carry around a list of all equivalent bases
--   For example: if you start with frame N and rotate about Nz to get A, then Nz == Az
type EquivBases = (HashSet (Basis, Basis))

------------------------- get equivilant bases ---------------------------
class HasEquivBases a where
  equivBases :: a -> EquivBases

instance HasEquivBases Bases where
  equivBases NewtonianBases = HS.empty
  equivBases (RotatedBases _ (RotSpeed _) _) = HS.empty
  equivBases f@(RotatedBases f0 (RotCoord rotVec) _) = case rotBases of
    [Basis frame xyz] -> if frame == f0
                         then HS.insert (Basis f0 xyz, Basis f xyz) (equivBases f0)
                         else HS.empty
    _ -> HS.empty
    where
      rotBases = HM.keys $ (\(Vec hm) -> hm) rotVec

instance HasEquivBases Basis where
  equivBases (Basis frame _) = equivBases frame
  equivBases (Cross b0 b1) = equivBases b0 `HS.union` equivBases b1


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

instance Hashable Bases where
  hash NewtonianBases = hash "NewtonianBases"
  hash (RotatedBases frame rot name) = hash "RotatedBases" `combine` hash frame `combine` hash rot `combine` hash name

instance Hashable Rotation where
  hash (RotCoord q)        = hash "RotCoord" `combine` hash q
  hash (RotSpeed v)        = hash "RotSpeed" `combine` hash v
--  hash (RotCoordSpeed q v) = hash "RotCoordSpeed" `combine` hash q `combine` hash v
  

------------------------- Num/Fractional instances ---------------------------------
-- if the input Sca is the result of unary negation, return the un-negated version
-- otherwise return Nothing
fromNeg :: Sca -> Maybe Sca
fromNeg (SNeg x) = Just x
fromNeg (SExpr (ENum (Negate x)) _) = Just $ SExpr x Nothing
fromNeg (SDot bb x) = case fromNeg x of Just x' -> Just (SDot bb x')
                                        Nothing -> Nothing
fromNeg _ = Nothing

instance Num Sca where
  (*) x y
    | isVal 0 x || isVal 0 y = 0
    | isVal 1 x = y
    | isVal 1 y = x
  (*) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> x' * y'
              (Nothing, Just y') -> negate (x  * y')
              (Just x', Nothing) -> negate (x' * y )
              _ -> case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' * y') Nothing
                                 _ -> SMul x y

  (+) x y
    | x == -y = 0
    | isVal 0 x = y
    | isVal 0 y = x
  (+) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> negate (x' + y')
              (Nothing, Just y') -> x  - y'
              (Just x', Nothing) -> y  - x'
              _ -> case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' + y') Nothing
                                 _ -> SAdd x y

  (-) x y
    | x == y = 0
    | isVal 0 x = -y
    | isVal 0 y = x
  (-) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> y' - x'
              (Nothing, Just y') -> x + y'
              (Just x', Nothing) -> negate (x' + y)
              _ -> case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' - y') Nothing
                                 _ -> SSub x y

  abs = error "abs not defined for Num Sca"
  signum = error "signum not defined for Num Sca"
  fromInteger = (\x -> x Nothing) . SExpr . fromInteger
  negate x = case fromNeg x of Nothing -> SNeg x
                               Just x' -> x'

instance Fractional Sca where
  (/) x y
    | isVal 1 y = x
    | isVal 0 y = error "Fractional Sca (/): divide by 0"
    | isVal 0 x = 0
  (/) x y = case (fromNeg x, fromNeg y) of
              (Just x', Just y') -> x' / y'
              (Nothing, Just y') -> negate (x  / y')
              (Just x', Nothing) -> negate (x' / y )
              _ -> case (x,y) of (SExpr x' _, SExpr y' _) -> SExpr (x' / y') Nothing
                                 _ -> SDiv x y

  fromRational = (\x -> x Nothing) . SExpr . fromRational

instance Num Vec where
  (+) (Vec x) (Vec y) = removeZeros $ Vec (HM.unionWith (+) x y)
  (-) (Vec x) (Vec y) = removeZeros $ Vec (HM.unionWith f x (HM.map negate y))
    where
      f x' y' = x' - (negate y')
  negate (Vec x) = removeZeros $ Vec (HM.map negate x)

  (*) = error "(*) not instanced for Vec"
  abs = error "abs not instanced for Vec"
  signum = error "signum not instanced for Vec"
  fromInteger 0 = zeroVec
  fromInteger _ = error "Num Vec's fromInteger only instanced for 0"


-------------------------- show instances ----------------------------------
showInfixBinary :: (Show a, Show b) => Int -> Int -> String -> a -> b -> ShowS
showInfixBinary d prec op u v = showParen (d > prec) $
                                showsPrec prec u .
                                showString op .
                                showsPrec prec v

showInfixBinaryNonAssoc :: (Show a, Show b) => Int -> Int -> String -> a -> b -> ShowS
showInfixBinaryNonAssoc d prec op u v = showParen (d > prec) $
                                        showsPrec (prec+1) u .
                                        showString op .
                                        showsPrec (prec+1) v

instance Show Sca where
  showsPrec d (SExpr x _) = showsPrec d x
  showsPrec d (SDot (b0,b1) x)
     | isVal 1 x = showInfixBinaryNonAssoc d 5 " . " b0 b1
     | otherwise = showsPrec d (SMul x (SDot (b0,b1) 1))
  showsPrec d (SMul x y) = showInfixBinary d 7 " * " x y
  showsPrec d (SDiv x y) = showInfixBinary d 7 " / " x y
  showsPrec d (SAdd x y) = showInfixBinary d 6 " + " x y
  showsPrec d (SSub x y) = showInfixBinary d 6 " - " x y
  showsPrec d (SNeg x) = showParen (d > prec) $
                         showString "-" .
                         showsPrec prec x
    where
      prec = 7

instance Show Vec where
  showsPrec d (Vec hm)
    | HM.null hm = showString "0>"
    | otherwise = showParen (d > 6) $
                   foldl f (showScaledBasis 6 (head hmlist)) (tail hmlist)
    where
      hmlist = HM.toList hm
      f acc (b,sca) = acc . showString " + " . showScaledBasis 6 (b,sca)
      showScaledBasis d' (b, sca)
        | isVal 1 sca = showsPrec d' b
        | otherwise = showParen (d' > prec) $
                      showsPrec prec sca .
                      showString " * " .
                      showsPrec prec b
        where
          prec = 7

instance Show Basis where
  showsPrec _ (Basis f xyz) = showString $ show f ++ show xyz
  showsPrec d (Cross b0 b1) = showInfixBinaryNonAssoc d 5 " x " b0 b1

instance Show XYZ where
  show X = "x>"
  show Y = "y>"
  show Z = "z>"

instance Show Bases where
  show NewtonianBases = "N"
  show (RotatedBases _ _ n) = n

instance Show Point where
  show = show . vecFromN0


-------------------- utils ---------------
vecFromN0 :: Point -> Vec
vecFromN0 N0 = 0
vecFromN0 (RelativePoint p0 v) = v + vecFromN0 p0

vecsFromN0 :: Point -> [Vec]
vecsFromN0 N0 = []
vecsFromN0 (RelativePoint p0 v) = v:(vecsFromN0 p0)

foldSca :: (Sca -> b -> b) -> b -> Sca -> b
foldSca f acc s@(SExpr _ _) = f s acc
foldSca f acc (SDot _ x)= foldSca f acc x
foldSca f acc (SNeg x) = foldSca f acc x
foldSca f acc (SAdd x y) = foldSca f (foldSca f acc y) x
foldSca f acc (SSub x y) = foldSca f (foldSca f acc y) x
foldSca f acc (SMul x y) = foldSca f (foldSca f acc y) x
foldSca f acc (SDiv x y) = foldSca f (foldSca f acc y) x

isVal :: Double -> Sca -> Bool
isVal x (SExpr e _) = Expr.isVal x e
isVal x (SNeg e) = isVal (-x) e
isVal _ _ = False

zeroVec :: Vec
zeroVec = Vec HM.empty

removeZeros :: Vec -> Vec
removeZeros (Vec hm) = Vec $ HM.filter (not . isVal 0) hm
--removeZeros (Vec hm) = trace ("\n-------------\nbefore: "++show hm ++ "\nafter:  "++show ret) $ Vec ret
--  where
--    ret = HM.filter (not . (isVal 0)) hm

isCoord, isSpeed, isTime :: Sca -> Bool
isCoord (SExpr (ESym (SymDependent _ 0 (Sym t))) (Just 0)) = ESym (Sym t) == time
isCoord _ = False

isSpeed (SExpr (ESym (SymDependent _ _ (Sym t))) (Just 1)) = ESym (Sym t) == time
isSpeed _ = False

isTime (SExpr t Nothing) = t == time
isTime _ = False

-- | the independent variable time used in taking time derivatives
time :: Expr Double
time = sym "t"
