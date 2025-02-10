{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PackageImports #-}
module Tridi.GL.Utils where

import Graphics.GLU ( gluLookAt )
import Graphics.GLU.Functions ()
import Graphics.GL.Internal.Shared
    ( glBegin,
      pattern GL_LINES, glEnd, glColor3d,
      pattern GL_QUADS, glVertex3d )
import "gl" Graphics.GL.Types (GLdouble)
import Control.Monad.IO.Class (MonadIO)
import Data.Foldable (traverse_)

import Orth.P3 (P3 (P3))

glParalelopiped :: (MonadIO m, Real n ) => P3 n -> P3 n -> P3 n -> P3 n -> m ()
glParalelopiped v dA dB dC
  = glBegin GL_LINES >> vertices (vParalelopiped v dA dB dC) >> glEnd
 >> glColor3d 0.5 0.5 0.5
 >> glBegin GL_QUADS >> vertices (vParalelopiped v dA dB dC) >> glEnd

glColParalelopiped :: (MonadIO m, Real t, Real n) => (t, t, t) -> P3 n -> P3 n -> P3 n -> P3 n -> m ()
glColParalelopiped (r,g,b) v dA dB dC
  = glColor3d (realToFrac r) (realToFrac g) (realToFrac b)
 >> glBegin GL_LINES >> vertices (vParalelopiped v dA dB dC) >> glEnd
 >> glColor3d (realToFrac r^2 ) (realToFrac g ^2) (realToFrac b^2)
 >> glBegin GL_QUADS >> vertices (vParalelopiped v dA dB dC) >> glEnd

vParalelogram :: Num n => P3 n -> P3 n -> P3 n -> [P3 n]
vParalelogram v dA dB = [v, v <> dA, v <> dA <> dB, v <> dB]

vParalelopiped :: Num n => P3 n -> P3 n -> P3 n -> P3 n -> [P3 n]
vParalelopiped v dA dB dC
  =  vParalelogram v dA dB
  <> vParalelogram v dB dC
  <> vParalelogram v dC dA
  <> vParalelogram (v <> dA) dB dC
  <> vParalelogram (v <> dB) dC dA
  <> vParalelogram (v <> dC) dA dB

vertex' :: (MonadIO m, Real n) => P3 n -> m ()
vertex' (P3 x y z) = glVertex3d (dCast x) (dCast z) (dCast y)

vertices ::  (MonadIO m, Real n) => [P3 n] -> m ()
vertices = traverse_ vertex'

dCast :: Real n => n -> GLdouble
dCast = realToFrac

lookAt' :: ( Real n) => P3 n -> P3 n -> IO ()
lookAt' (P3 fx fy fz) (P3 dx dy dz) = gluLookAt (dCast fx) (dCast fz) (-dCast fy)
                                                (dCast tx) (dCast tz) (-dCast ty)
                                                0          1          0
  where
    (tx, ty, tz) = (fx + dx, fy + dy, fz + dz)
