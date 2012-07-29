-- {-# OPTIONS_GHC -Wall #-}

module Kitesys where

import System
import Frames
import Types

n = NewtonianFrame "N"

rArm = param "R"
delta = coord "d"

carouselFrame = rotZ n delta "C"

lineWy = speed "lwz"
lineWz = speed "lwy"

lineFrame = RotatedFrame carouselFrame (RotSpeed (0,lineWy,lineWz)) "L"

rLine = param "r"

jx = param "Jx"
jy = param "Jy"
jz = param "Jz"

wx = speed "wx"
wy = speed "wy"
wz = speed "wz"

m = param "m"
kiteFrame = RotatedFrame n (RotSpeed (wx,wy,wz)) "K"
r_n02k = scaleBasis rArm (Basis carouselFrame X) + scaleBasis rLine (Basis lineFrame X)

fx = param "Fx"
fy = param "Fy"
fz = param "Fz"
mx = param "Tx"
my = param "Ty"
mz = param "Tz"
force =  Force  $ scaleBasis fx (Basis n X) + scaleBasis fy (Basis n Y) + scaleBasis fz (Basis n Z)
torque = Torque $ scaleBasis mx (Basis n X) + scaleBasis my (Basis n Y) + scaleBasis mz (Basis n Z)

kite = RigidBody m (simpleDyadic jx jy jz kiteFrame) r_n02k kiteFrame force torque

kitesys :: IO ()
kitesys = do
  print kite
  putStrLn "kane's eqs: "
  mapM_ print $ kaneEqs [kite] [wx,wy,wz,ddt delta]
