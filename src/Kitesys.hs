-- {-# OPTIONS_GHC -Wall #-}

module Kitesys where

import Classy

model :: ClassySystem
model = runClassyState $ do
  n <- newtonianBases

  rArm <- addParam "R"
  delta <- addCoord "d"

  carouselBases <- rotZ n delta "C"

  lineWy <- addSpeed "lwz"
  lineWz <- addSpeed "lwy"

  let lineBases = basesWithAngVel carouselBases (0,lineWy,lineWz) "L"

  rLine <- addParam "r"

  jx <- addParam "Jx"
  jy <- addParam "Jy"
  jz <- addParam "Jz"

  wx <- addSpeed "wx"
  wy <- addSpeed "wy"
  wz <- addSpeed "wz"

  m <- addParam "m"
  let kiteBases = basesWithAngVel n (wx,wy,wz) "K"
  let r'n0'k = relativePoint N0 $ xVec rArm carouselBases + xVec rLine lineBases

  fx <- addParam "Fx"
  fy <- addParam "Fy"
  fz <- addParam "Fz"
  mx <- addParam "Tx"
  my <- addParam "Ty"
  mz <- addParam "Tz"

  kite <- addRigidBody m (simpleDyadic jx jy jz kiteBases) r'n0'k kiteBases
  addForce kite r'n0'k (xyzVec (fx,fy,fz) kiteBases)
  addMoment kite (xyzVec (mx,my,mz) kiteBases)


run :: IO ()
run = do
  print model
  putStrLn "\n--------------- kane's eqs: ------------------"
  print $ kanes model
