{-# OPTIONS_GHC -Wall #-}

module Sym( Sca(..)
          , Vec(..)
          , Coord(..)
          , Speed(..)
          , Accel(..)
          , ddt
          , ddtV
          , partial
          , go
          ) where

data Coord = Coord String deriving Eq
data Speed = Speed String
           | DdtCoord Coord deriving Eq
data Accel = Accel String
           | DdtSpeed Speed deriving Eq
                                     
instance Show Coord where
  show (Coord x) = x
instance Show Speed where
  show (Speed x) = x
  show (DdtCoord x) = show x++"'"
instance Show Accel where
  show (Accel x) = x
  show (DdtSpeed x) = show x++"'"

data Sca = SCoord Coord
         | SSpeed Speed
         | SAccel Accel
         | SParam String
         | SNum Double
         | SMul Sca Sca
         | SDiv Sca Sca
         | SAdd Sca Sca
         | SSub Sca Sca
         | SNeg Sca
         | SDot Vec Vec
         | SZero
         | SOne deriving Eq

instance Show Sca where
  show (SCoord x) = show x
  show (SSpeed x) = show x
  show (SAccel x) = show x
  show (SParam x) = x
  show (SNum x) = show x
  show (SMul x y) = "( " ++ show x ++ " * " ++ show y ++ " )"
  show (SDiv x y) = "( " ++ show x ++ " / " ++ show y ++ " )"
  show (SAdd x y) = "( " ++ show x ++ " + " ++ show y ++ " )"
  show (SSub x y) = "( " ++ show x ++ " - " ++ show y ++ " )"
  show (SNeg x) = "( - " ++ show x ++ " )"
  show (SDot _ _ ) = error "maybe do something about this?"
  show (SZero) = "0"
  show (SOne) = "1"
  

instance Num Sca where
  SZero * _ = SZero
  _ * SZero = SZero
  (SNum x) * (SNum y) = SNum (x*y)
  x * y = SMul x y
  
  SZero + y = y
  x + SZero = x
  (SNum x) + (SNum y) = SNum (x+y)
  x + y = SAdd x y
  
  SZero - y = SNeg y
  x - SZero = x
  (SNum x) - (SNum y) = SNum (x-y)
  x - y = SSub x y

  abs _ = error "Shut it"
  signum _ = error "you are a goon"
  fromInteger k = SNum (fromInteger k)

instance Fractional Sca where
  _ / SZero = error "_ / SZero"
  SZero / _ = SZero
  (SNum x) / (SNum y) = SNum (x/y)
  x / y = SDiv x y
  
  fromRational x = SNum (fromRational x)
  

ddt :: Sca -> Sca
ddt (SCoord q) = SSpeed (DdtCoord q)
ddt (SSpeed u) = SAccel (DdtSpeed u)
ddt (SAccel _) = error "whyyyy is this happening?"
ddt (SParam _) = SZero
ddt (SNum _) = SZero
ddt SZero = SZero
ddt SOne = SZero
ddt (SMul x y) = x' * y + x * y'
  where
    x' = ddt x
    y' = ddt y
ddt (SDiv x y) = (x'*y - x*y') / (y*y)
  where
    x' = ddt x
    y' = ddt y
ddt (SAdd x y) = ddt x + ddt y
ddt (SSub x y) = ddt x - ddt y
ddt (SNeg x) = SNeg (ddt x)
ddt (SDot _ _) = error "hmmm... ddt (SDot Vec Vec) you say?"


-- partial will FAIL if you are taking derivitive
-- w.r.t. a coordinate because i'm assuming
-- partial (f*bx>) p == (partial f p)*bx>
partial :: Sca -> Sca -> Sca
partial x y@(SCoord _) = unsafePartial x y
partial x y@(SSpeed _) = unsafePartial x y
partial x y@(SAccel _) = unsafePartial x y
partial x y@(SParam _) = unsafePartial x y
partial _ (SNum _)   = error "unsafe partial intercept!!"
partial _ SZero      = error "unsafe partial intercept!!"
partial _ SOne       = error "unsafe partial intercept!!"
partial _ (SMul _ _) = error "unsafe partial intercept!!"
partial _ (SDiv _ _) = error "unsafe partial intercept!!"
partial _ (SAdd _ _) = error "unsafe partial intercept!!"
partial _ (SSub _ _) = error "unsafe partial intercept!!"
partial _ (SNeg _)   = error "unsafe partial intercept!!"
partial _ (SDot _ _) = error "unsafe partial intercept!!"

unsafePartial :: Sca -> Sca -> Sca
unsafePartial x@(SCoord _) p = if x == p then SOne else SZero
unsafePartial x@(SSpeed _) p = if x == p then SOne else SZero
unsafePartial x@(SAccel _) p = if x == p then SOne else SZero
unsafePartial x@(SParam _) p = if x == p then SOne else SZero
unsafePartial (SNum _) _ = SZero
unsafePartial SZero _ = SZero
unsafePartial SOne _ = SZero
unsafePartial (SMul x y) p = x' * y + x * y'
  where
    x' = unsafePartial x p
    y' = unsafePartial y p
unsafePartial (SDiv x y) p = (x'*y - x*y') / (y*y)
  where
    x' = unsafePartial x p
    y' = unsafePartial y p
unsafePartial (SAdd x y) p = unsafePartial x p + unsafePartial y p
unsafePartial (SSub x y) p = unsafePartial x p - unsafePartial y p
unsafePartial (SNeg x) p = SNeg (unsafePartial x p)
unsafePartial (SDot _ _) _ = error "hmmm... unsafePartial (SDot Vec Vec) you say?"




data Vec = VBasis Basis Sca
         | VZero
         | VSum Vec Vec
         | VSub Vec Vec
         | VNeg Vec
         | VCross Vec Vec deriving Eq
instance Show Vec where
  show (VBasis b SOne) = show b
  show (VBasis b x) = show x ++ "*" ++ show b
  show VZero = "0>"
  show (VSum vx vy) = "( " ++ show vx ++ " + " ++ show vy ++ " )"
  show (VSub vx vy) = "( " ++ show vx ++ " - " ++ show vy ++ " )"
  show (VNeg v) = "( -" ++ show v ++ " )"
  show (VCross vx vy) = "( " ++ show vx ++ " x " ++ show vy ++ " )"

data XYZ = X | Y | Z deriving Eq
instance Show XYZ where
  show X = "x>"
  show Y = "y>"
  show Z = "z>"
data Basis = Basis XYZ Frame deriving Eq

instance Show Basis where
  show (Basis xyz frame) = show frame ++ show xyz
  
data Frame = NewtonianFrame String
           | RFrame Frame Rotation String deriving Eq
instance Show Frame where
  show (NewtonianFrame n) = n
  show (RFrame _ _ n) = n

data Rotation = XRot Coord
              | YRot Coord
              | ZRot Coord
              | WRot Vec deriving (Show, Eq)

angVel :: Frame -> Rotation -> Vec
angVel _ (WRot w) = w
angVel f (XRot q) = VBasis (Basis X f) (ddt (SCoord q))
angVel f (YRot q) = VBasis (Basis Y f) (ddt (SCoord q))
angVel f (ZRot q) = VBasis (Basis Z f) (ddt (SCoord q))

angVelWrtN :: Frame -> Vec
angVelWrtN (NewtonianFrame _) = VZero
angVelWrtN (RFrame frame0 rot _) = vSum (angVelWrtN frame0) (angVel frame0 rot)

ddtV :: Frame -> Vec -> Vec
ddtV _ VZero = VZero
ddtV f (VCross vx vy) = vSum (vCross (ddtV f vx) vy) (vCross vx (ddtV f vy))
ddtV f (VSum   vx vy) = vSum (ddtV f vx) (ddtV f vy)
ddtV f (VSub   vx vy) = vSub (ddtV f vx) (ddtV f vy)
ddtV f (VNeg   vx)    = vNeg (ddtV f vx)
ddtV f v@(VBasis b@(Basis _ bf) sca) 
  | f == bf   = ddtN
  | otherwise = vSum ddtN wCrossV
  where
    ddtN 
      | ddtS == SZero = VZero
      | otherwise     = VBasis b ddtS
      where ddtS = ddt sca
    wCrossV = vCross w v
      where
        w = vSub (angVelWrtN bf) (angVelWrtN f)


-- partial will FAIL if you are taking derivitive
-- w.r.t. a coordinate because i'm assuming
-- partial (f*bx>) p == (partial f p)*bx>
partialV :: Vec -> Sca -> Vec
partialV VZero _ = VZero
partialV (VCross vx vy) p = vSum (vCross (partialV vx p) vy) (vCross vx (partialV vy p))
partialV (VSum   vx vy) p = vSum (partialV vx p) (partialV vy p)
partialV (VSub   vx vy) p = vSub (partialV vx p) (partialV vy p)
partialV (VNeg   vx)    p = vNeg (partialV vx p)
partialV (VBasis b sca) p
  | psp == SZero = VZero
  | otherwise    = VBasis b psp
  where
    psp = partial sca p

vCross :: Vec -> Vec -> Vec
vCross VZero _ = VZero
vCross _ VZero = VZero
vCross x y = VCross x y

vSum :: Vec -> Vec -> Vec
vSum VZero y = y
vSum x VZero = x
vSum x y = VSum x y

vSub :: Vec -> Vec -> Vec
vSub VZero y = VNeg y
vSub x VZero = x
vSub x y = VSub x y

vNeg :: Vec -> Vec
vNeg VZero = VZero
vNeg x = VNeg x

rotZ :: Frame -> Coord -> String -> Frame
rotZ f q name = RFrame f (ZRot q) name

go :: IO ()
go = do
  let q = Coord "q"
      q' = ddt (SCoord q)
      
      n = NewtonianFrame "N"
      b = rotZ n q "B"
      
      len = SNum 1.3
      r_n02p = VBasis (Basis X b) len
      n_v_p = ddtV n r_n02p
      n_a_p = ddtV n n_v_p
  
  print r_n02p
  print n_v_p
  print (partialV n_v_p q')
  print n_a_p
