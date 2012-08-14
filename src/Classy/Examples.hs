{-# OPTIONS_GHC -Wall #-}

module Classy.Examples ( simple
                       ) where

import Classy

simple :: IO ()
simple = do
  let sys = runClassyState $ do
        n <- newtonianBases
        jx <- addParam "Jx"
        jy <- addParam "Jy"
        jz <- addParam "Jz"
        
        wx <- addSpeed "wx"
        wy <- addSpeed "wy"
        wz <- addSpeed "wz"
        
        mx <- addParam "Tx"
        my <- addParam "Ty"
        mz <- addParam "Tz"
        
        let b = basesWithAngVel n (wx,wy,wz) "B"
        body <- addRigidBody 1 (simpleDyadic jx jy jz b) N0 b
        addMoment body (xyzVec (mx,my,mz) b)
        --addMoment body (xyzVec (mx,my,mz) n)

  putStrLn "kane's eqs: "
  print $ kanes sys
