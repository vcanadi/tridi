{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Tridi.GL.Observer where

{- Main loop template that handles opengl details -}

import GHC.Generics
import Data.Function ((&))
import Tridi.GL.Input (Input, inputR, inputV)

import Orth.Quaternion (rotateAround)
import Orth.P3


-- | From where, and in which direction it is observing
data Observer n = Observer
  { _obsFrom :: P3 n
  , _obsDir  :: P3 n
  } deriving (Show, Generic)

-- | Replacement for Data.def
defObserver :: Num n => Observer n
defObserver = Observer (P3 (-10) 0 0) (P3 1 0 0)

-- | Replacement for lenses' `obsFrom %~`
overObsFrom :: (P3 n -> P3 n) -> (Observer n -> Observer n)
overObsFrom f o = Observer (f $ _obsFrom o) (_obsDir o)

-- | Replacement for lenses' `obsDir %~`
overObsDir :: (P3 n -> P3 n) -> (Observer n -> Observer n)
overObsDir f o = Observer (_obsFrom o) (f $ _obsDir o)

obsDirLeft :: (Num n) => Observer n -> P3 n
obsDirLeft o =  P3 0 0 1 `p3Cross` _obsDir o

obsDirUp :: (Num n) => Observer n -> P3 n
obsDirUp o = _obsDir o `p3Cross` obsDirLeft o

-- | Constants. TODO: Move to Config module
moveDelta, rotHDelta, rotVDelta :: Fractional n => n
moveDelta = 0.3
rotHDelta = 0.05
rotVDelta = 0.05

-- | Move Observer by positional delta
moveObs :: (Fractional n) => P3 n -> Observer n -> Observer n
moveObs d o
  = o & overObsFrom ( <> p3Scale moveDelta
                          (   p3Scale (d `p3Dot` P3 1 0 0) (_obsDir o)
                           <> p3Scale (d `p3Dot` P3 0 1 0) (obsDirLeft o)
                           <> p3Scale (d `p3Dot` P3 0 0 1) (P3 0 0 1)
                          )
                    )

-- | Rotate observer by vertical and horisontal rotational delta
rotObs :: (RealFloat n) => (n, n) -> Observer n -> Observer n
rotObs (h, v) o
  = o & overObsDir (  rotateAround (obsDirLeft o) (-v * rotVDelta)
                   .  rotateAround (P3 0 0 1)     (h * rotHDelta)
                   )

applyInput :: (RealFloat n) => Input -> Observer n -> Observer n
applyInput input = rotObs (inputR input)
                 . moveObs (inputV input)
