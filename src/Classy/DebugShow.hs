{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleInstances #-}

module Classy.DebugShow ( debugShow
                        , debugPrint
                        ) where

import Data.List ( intercalate )
import Data.HashMap.Lazy ( HashMap )
import qualified Data.HashMap.Lazy as HM
import Data.HashSet ( HashSet )
import qualified Data.HashSet as HS

import Classy.Types

debugPrint :: DebugShow a => Maybe Int -> a -> IO ()
debugPrint mi = putStrLn . (debugShow mi)

class Show a => DebugShow a where
  debugShow :: Maybe Int -> a -> String

dec :: Maybe Int -> Maybe Int
dec = fmap (\k -> k - 1)

debugShow1 :: DebugShow a => Maybe Int -> String -> a -> String
debugShow1 (Just 0) name x = name ++ " (" ++ show x ++ ")"
debugShow1 mi name x = name ++ " (" ++ debugShow (dec mi) x ++ ")"

debugShow2 :: (DebugShow a, DebugShow b) => Maybe Int -> String -> a -> b -> String
debugShow2 mi@(Just 0) name x y = debugShow1 mi name x ++ " (" ++ show y ++ ")"
debugShow2 mi name x y = debugShow1 mi name x ++ " (" ++ debugShow (dec mi) y ++ ")"

debugShow3 :: (DebugShow a, DebugShow b, DebugShow c) => Maybe Int -> String -> a -> b -> c -> String
debugShow3 mi@(Just 0) name x y z = debugShow2 mi name x y ++ " (" ++ show z ++ ")"
debugShow3 mi name x y z = debugShow2 mi name x y ++ " (" ++ debugShow (dec mi) z ++ ")"

debugShow4 :: (DebugShow a, DebugShow b, DebugShow c, DebugShow d)
              => Maybe Int -> String -> a -> b -> c -> d -> String
debugShow4 mi@(Just 0) name x y z w = debugShow3 mi name x y z ++ " (" ++ show w ++ ")"
debugShow4 mi name x y z w = debugShow3 mi name x y z ++ " (" ++ debugShow (dec mi) w ++ ")"

instance DebugShow Char where debugShow _ = show
instance DebugShow [Char] where debugShow _ = show

instance (DebugShow a, DebugShow b) => DebugShow (a,b) where
  debugShow mi (x,y) = "(" ++ debugShow mi x ++ "," ++ debugShow mi y ++ ")"

instance (DebugShow a, DebugShow b, DebugShow c) => DebugShow (a,b,c) where
  debugShow mi (x,y,z) = "(" ++ debugShow mi x ++ "," ++ debugShow mi y ++ "," ++ debugShow mi z ++ ")"

debugShowList :: DebugShow a => Maybe Int -> [a] -> String
debugShowList mi xs = '[' : intercalate "," (map (debugShow mi) xs) ++ "]"

instance (DebugShow a, DebugShow b) => DebugShow (HashMap a b) where
  debugShow mi hm = "fromList " ++ debugShowList mi (HM.toList hm)

instance DebugShow a => DebugShow (HashSet a) where
  debugShow mi hs = "fromList " ++ debugShowList mi (HS.toList hs)

instance DebugShow Ordering where debugShow _ = show

instance DebugShow Sca where
  debugShow _ x = show x -- always fully show scalars
--  debugShow (Just 0) x = show x
--  debugShow _ (SExpr e mi) = "SExpr " ++ show e ++ " " ++ show mi
--  debugShow k (SDot b0b1 sca) = debugShow2 k "SDot" b0b1 sca
--  debugShow k (SNeg x) = debugShow1 k "SNeg" x
--  debugShow k (SAdd x y) = debugShow2 k "SAdd" x y
--  debugShow k (SSub x y) = debugShow2 k "SSub" x y
--  debugShow k (SMul x y) = debugShow2 k "SMul" x y
--  debugShow k (SDiv x y) = debugShow2 k "SDiv" x y

instance DebugShow Basis where
  debugShow mi (Basis bases xyz) = debugShow2 mi "Basis" bases xyz
  debugShow mi (Cross b0 b1) = debugShow2 mi "Cross" b0 b1

instance DebugShow Point where
  debugShow _ N0 = "N0"
  debugShow mi (RelativePoint p v) = debugShow2 mi "RelativePoint" p v

instance DebugShow Vec where
  debugShow _ x = show x -- always fully show vectors
--  debugShow mi (Vec hm) = debugShow1 mi "Vec" hm

instance DebugShow XYZ where
  debugShow _ X = "X"
  debugShow _ Y = "Y"
  debugShow _ Z = "Z"

instance DebugShow Bases where
  debugShow _ NewtonianBases = "NewtonianBases"
  debugShow mi (RotatedBases x y z) = debugShow3 mi "RotatedBases" x y z

instance DebugShow Rotation where
  debugShow mi (RotSpeed xyz) = debugShow1 mi "RotSpeed" xyz
  debugShow mi (RotCoord v) = debugShow1 mi "RotCoord" v

instance DebugShow Dyad where
  debugShow mi (Dyad x y z) = debugShow3 mi "Dyad" x y z

instance DebugShow Dyadic where
  debugShow mi (Dyadic xs) = debugShow1 mi "Dyadic" xs

instance DebugShow a => DebugShow (Equation a) where
  debugShow mi (Equation x ord y) = debugShow3 mi "Equation" x ord y

instance DebugShow Moments where
  debugShow mi (Moments vs) = "Moments " ++ debugShowList mi vs

instance DebugShow Forces where
  debugShow mi (Forces vs) = "Forces " ++ debugShowList mi vs

instance DebugShow Body where
  debugShow mi (Particle x y) = debugShow2 mi "Particle" x y
  debugShow mi (RigidBody x y z w) = debugShow4 mi "RigidBody" x y z w
