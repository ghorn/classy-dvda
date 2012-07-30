-- {-# OPTIONS_GHC -Wall #-}

module Kitesys where

import Classy

n = newtonianFrame

rArm = param "R"
delta = coord "d"

carouselFrame = rotZ n delta "C"

lineWy = speed "lwz"
lineWz = speed "lwy"

lineFrame = frameWithAngVel carouselFrame (0,lineWy,lineWz) "L"

rLine = param "r"

jx = param "Jx"
jy = param "Jy"
jz = param "Jz"

wx = speed "wx"
wy = speed "wy"
wz = speed "wz"

m = param "m"
kiteFrame = frameWithAngVel n (wx,wy,wz) "K"
r'n0'k = xVec rArm carouselFrame + xVec rLine lineFrame

fx = param "Fx"
fy = param "Fy"
fz = param "Fz"
mx = param "Tx"
my = param "Ty"
mz = param "Tz"
force =  Force  $ xyzVec (fx,fy,fz) kiteFrame
torque = Torque $ xyzVec (mx,my,mz) kiteFrame

kite = RigidBody m (simpleDyadic jx jy jz kiteFrame) r'n0'k kiteFrame force torque

run :: IO ()
run = do
  print kite
  putStrLn "kane's eqs: "
  print $ kaneEqs [kite] [wx,wy,wz,ddt delta]
