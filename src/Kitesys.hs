{-# OPTIONS_GHC -Wall #-}

module Kitesys where

import Classy
--import Classy.State hiding ( run )
--import Control.Monad.State
--import qualified Data.HashMap.Lazy as HM

kitesys :: IO System
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


simpleKitesys :: IO System
simpleKitesys = getSystemT $ do
  n <- newtonianBases

  rArm <- addParam "R"
  delta <- addCoord "d"
  _ <- derivIsSpeed delta

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

--  cs <- get
--  liftIO $ debugPrint Nothing (csBases cs)

  liftIO $ print $ kineticEnergy kite
  
  addForce kite r'n0'k (xyzVec (fx,fy,fz) kiteBases)
  addMoment kite (xyzVec (mx,my,mz) kiteBases)


run' :: IO ()
run' = do
  sys <- simpleKitesys
  putStrLn "\n--------------- kane's eqs: ------------------"
  print $ kanes sys




cartesianKitesys :: IO System
cartesianKitesys = getSystemT $ do
  n <- newtonianBases

  rArm <- addParam "rArm"
  delta <- addCoord "d"
  _ <- derivIsSpeed delta

  carouselBases <- rotZ n delta "C"

  x <- addCoord "x"
  y <- addCoord "y"
  z <- addCoord "z"
  [x',y',z'] <- mapM derivIsSpeed [x,y,z]

  r <- addCoord "r"

  jx <- addParam "Jx"
  jy <- addParam "Jy"
  jz <- addParam "Jz"

  wx <- addSpeed "wx"
  wy <- addSpeed "wy"
  wz <- addSpeed "wz"

  m <- addParam "m"
  kiteBases <- basesWithAngVel n (wx,wy,wz) "B"
  let r'n0'k = relativePoint N0 $ xyzVec ((rArm + x), y, z) carouselBases

  tension <- addParam "T"
  fx <- addParam "Fx"
  fy <- addParam "Fy"
  fz <- addParam "Fz"
  mx <- addParam "Tx"
  my <- addParam "Ty"
  mz <- addParam "Tz"

  -- carousel arm rigid body
--  cJxx <- addParam "cJxx"
--  cMass <- addParam "cm"
--  let armCm = relativePoint N0 $ xVec (rArm/2) carouselBases
--  _ <- addRigidBody cMass (simpleDyadic cJxx 0 0 carouselBases) armCm carouselBases

  -- kite rigid body
  kite <- addRigidBody m (simpleDyadic jx jy jz kiteBases) r'n0'k kiteBases

  -- external forces/torques
  addForce kite r'n0'k (xyzVec (fx,fy,fz) kiteBases)
  addMoment kite (xyzVec (mx,my,mz) kiteBases)

  -- constraint force
  addForce kite r'n0'k (xyzVec (-tension*x/r, -tension*y/r, -tension*z/r) carouselBases)

  liftIO $ print $ generalizedEffectiveForce x' kite
  liftIO $ print $ generalizedEffectiveForce y' kite
  liftIO $ print $ generalizedEffectiveForce z' kite


go :: IO ()
go = do
  sys <- cartesianKitesys
--  putStrLn "\n--------------- sys: ------------------"
--  print $ sys
  putStrLn "\n--------------- kane's eqs: ------------------"
  print $ kanes sys
