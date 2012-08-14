-- {-# OPTIONS_GHC -Wall #-}

module Kitesys where

import Classy
import Classy.State hiding ( run )
import Control.Monad.State

kitesys :: IO ClassySystem
kitesys = getSystemT $ do
  n <- newtonianBases

  rArm <- addParam "R"
  delta <- addCoord "d"

  carouselBases <- rotZ n delta "C"

  lineWy <- addSpeed "lwz"
  lineWz <- addSpeed "lwy"

  lineBases <- basesWithAngVel carouselBases (0,lineWy,lineWz) "L"

  rLine <- addParam "r"

  jx <- addParam "Jx"
  jy <- addParam "Jy"
  jz <- addParam "Jz"

  wx <- addSpeed "wx"
  wy <- addSpeed "wy"
  wz <- addSpeed "wz"

  m <- addParam "m"
  kiteBases <- basesWithAngVel n (wx,wy,wz) "K"
  let r'n0'k = relativePoint N0 $ xVec rArm carouselBases + xVec rLine lineBases

  fx <- addParam "Fx"
  fy <- addParam "Fy"
  fz <- addParam "Fz"
  mx <- addParam "Tx"
  my <- addParam "Ty"
  mz <- addParam "Tz"

  kite <- addRigidBody m (simpleDyadic jx jy jz kiteBases) r'n0'k kiteBases

  liftIO $ print $ kineticEnergy kite
  
  addForce kite r'n0'k (xyzVec (fx,fy,fz) kiteBases)
  addMoment kite (xyzVec (mx,my,mz) kiteBases)


run :: IO ()
run = do
  sys <- kitesys
  putStrLn "\n--------------- kane's eqs: ------------------"
  print $ kanes sys


simpleKitesys :: IO ClassySystem
simpleKitesys = getSystemT $ do
  n <- newtonianBases

  rArm <- addParam "R"
  delta <- addCoord "d"
  derivIsSpeed delta

  carouselBases <- rotZ n delta "C"

--  lineWy <- addSpeed "lwz"
--  lineWz <- addSpeed "lwy"

  let lineBases = carouselBases -- basesWithAngVel carouselBases (0,lineWy,lineWz) "L"

  rLine <- addParam "r"

  jx <- addParam "Jx"
  jy <- addParam "Jy"
  jz <- addParam "Jz"

  wx <- addSpeed "wx"
  wy <- addSpeed "wy"
  wz <- addSpeed "wz"

  m <- addParam "m"
  kiteBases <- basesWithAngVel n (wx,wy,wz) "K"
  let r'n0'k = relativePoint N0 $ xVec rArm carouselBases + xVec rLine lineBases

  fx <- addParam "Fx"
  fy <- addParam "Fy"
  fz <- addParam "Fz"
  mx <- addParam "Tx"
  my <- addParam "Ty"
  mz <- addParam "Tz"

  kite <- addRigidBody m (simpleDyadic jx jy jz kiteBases) r'n0'k kiteBases

  cs <- get
--  liftIO $ debugPrint Nothing (csBases cs)

  liftIO $ print $ kineticEnergy kite
  
  addForce kite r'n0'k (xyzVec (fx,fy,fz) kiteBases)
  addMoment kite (xyzVec (mx,my,mz) kiteBases)


run' :: IO ()
run' = do
  sys <- simpleKitesys
  putStrLn "\n--------------- kane's eqs: ------------------"
  print $ kanes sys
