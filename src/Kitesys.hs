{-# OPTIONS_GHC -Wall #-}

module Kitesys where

import Classy
--import Classy.State hiding ( run )
--import Control.Monad.State
--import qualified Data.HashMap.Lazy as HM

carouselSys :: IO System
carouselSys = getSystemT $ do
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
  cJxx <- addParam "cJxx"
  cMass <- addParam "cm"
  let armCm = relativePoint N0 $ xVec (rArm/2) carouselBases
  _ <- addRigidBody cMass (simpleDyadic cJxx 0 0 carouselBases) armCm carouselBases

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


carousel :: IO ()
carousel = do
  sys <- carouselSys
--  putStrLn "\n--------------- sys: ------------------"
--  print $ sys
  putStrLn "\n--------------- kane's eqs: ------------------"
  print $ kanes sys



crosswindSys :: IO System
crosswindSys = getSystemT $ do
  n <- newtonianBases

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
  let r'n0'k = relativePoint N0 $ xyzVec (x, y, z) n

  tension <- addParam "T"
  fx <- addParam "Fx"
  fy <- addParam "Fy"
  fz <- addParam "Fz"
  mx <- addParam "Mx"
  my <- addParam "My"
  mz <- addParam "Mz"

  -- kite rigid body
  kite <- addRigidBody m (simpleDyadic jx jy jz kiteBases) r'n0'k kiteBases

  -- external forces/torques
  addForce kite r'n0'k (xyzVec (fx,fy,fz) kiteBases)
  addMoment kite (xyzVec (mx,my,mz) kiteBases)

  -- constraint force
  addForce kite r'n0'k (xyzVec (-tension*x/r, -tension*y/r, -tension*z/r) n)

  liftIO $ print $ generalizedEffectiveForce x' kite
  liftIO $ print $ generalizedEffectiveForce y' kite
  liftIO $ print $ generalizedEffectiveForce z' kite

crosswind :: IO ()
crosswind = do
  sys <- crosswindSys
--  putStrLn "\n--------------- sys: ------------------"
--  print $ sys
  putStrLn "\n--------------- kane's eqs: ------------------"
  let keq = kanes sys
  print keq
