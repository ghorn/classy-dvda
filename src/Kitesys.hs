-- {-# OPTIONS_GHC -Wall #-}

module Kitesys where

import Classy

n = newtonianBases

rArm = param "R"
delta = coord "d"

carouselBases = rotZ n delta "C"

lineWy = speed "lwz"
lineWz = speed "lwy"

lineBases = frameWithAngVel carouselBases (0,lineWy,lineWz) "L"

rLine = param "r"

jx = param "Jx"
jy = param "Jy"
jz = param "Jz"

wx = speed "wx"
wy = speed "wy"
wz = speed "wz"

m = param "m"
kiteBases = frameWithAngVel n (wx,wy,wz) "K"
r'n0'k = RelativePoint N0 $ xVec rArm carouselBases + xVec rLine lineBases

fx = param "Fx"
fy = param "Fy"
fz = param "Fz"
mx = param "Tx"
my = param "Ty"
mz = param "Tz"
force =  Forces  [(r'n0'k, xyzVec (fx,fy,fz) kiteBases)]
torque = Torque $ xyzVec (mx,my,mz) kiteBases

kite = RigidBody m (simpleDyadic jx jy jz kiteBases) r'n0'k kiteBases force torque

run :: IO ()
run = do
  print kite
  putStrLn "kane's eqs: "
  print $ kaneEqs [kite] [wx,wy,wz,ddt delta]
