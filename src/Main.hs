module Main where

import Control.Arrow ((>>>))
import Data.Foldable(traverse_)

import Orth.P3 (p3Sub, P3(P3))

import Tridi.GL.Loop (bracketGLFWWin)
import Tridi.GL.Utils (glColParalelopiped)

d_i = 15
d_j = 15
d_k = 15

main :: IO ()
main = bracketGLFWWin (100, 100) $ \_ _ -> (`traverse_` ((,,) <$> [0..d_i] <*> [0..d_j] <*> [0..d_k])) $ \(i,j,k) ->
    glColParalelopiped (bright i d_i,bright j d_j,bright k d_k) (P3 (2*i) (2*j) (2*k)) (P3 1 0 0)  (P3 0 1 0) (P3 0 0 1)


bright a b = 0.0 + 1.0 * (a/b)

