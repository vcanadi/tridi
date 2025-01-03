{-# LANGUAGE PatternSynonyms #-}
module Tridi.GL.Loop where

{- Main loop template that handles opengl details -}

import Graphics.GLU ( gluOrtho2D, gluPerspective )
import Graphics.GLU.Functions ()
import Graphics.GL.Internal.Shared
    ( pattern GL_COLOR_BUFFER_BIT,
      pattern GL_DEPTH_BUFFER_BIT,
      pattern GL_DEPTH_TEST,
      pattern GL_LEQUAL,
      pattern GL_MODELVIEW,
      pattern GL_NICEST,
      pattern GL_PERSPECTIVE_CORRECTION_HINT,
      pattern GL_PROJECTION,
      glClear,
      glClearColor,
      glClearDepth,
      glDepthFunc,
      glEnable,
      glFlush,
      glHint,
      glLoadIdentity,
      glMatrixMode,
      glViewport )
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent (threadDelay)
import System.Exit (exitSuccess)
import Control.Monad (when, void)
import Data.IORef (newIORef, IORef, modifyIORef, readIORef)

import Tridi.GL.Observer (Observer (Observer), defObserver, applyInput)
import Tridi.GL.Input (Input, defInput, pressKey, releaseKey, readInputKey, inputR)
import Tridi.GL.Utils (lookAt')
import Data.Bits ((.|.))

-- | Take care of GLFW initialization, creating a window, running main loop and cleaning up afterwards
bracketGLFWWin :: (Int, Int) -> (Integer -> GLFW.Window -> IO ()) -> IO ()
bracketGLFWWin (winW, winH) act = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  winMb <- GLFW.createWindow winW winH "win0" Nothing Nothing
  case winMb of
    Nothing  -> putStrLn "Unable to create window"
    Just win -> do

      -- | Initialize Input IORef. This is stateless and updated from keyboard by GLFW
      inputS <- newIORef defInput

      GLFW.makeContextCurrent (Just win)
      GLFW.setWindowSizeCallback win (Just resizeWindow)
      -- | Give input to GLFW for filling with keyboard events
      GLFW.setKeyCallback win (Just $ keyCallback inputS)
      GLFW.setWindowCloseCallback win (Just shutdown)
      gluOrtho2D 0 (realToFrac winW) (realToFrac winH) 0

      -- | Start main loop with references to input
      loop win inputS

  where
    -- | Main loop (update function)
    loop :: GLFW.Window -> IORef Input -> IO ()
    loop win inputS = loop' 0 defObserver
      where
        loop' :: RealFloat n => Integer -> Observer n -> IO ()
        loop' step obs = do
          shouldContinue <- not <$> GLFW.windowShouldClose win
          when shouldContinue $ do
            initGL win

            -- | Read from Input what was given form GLFW and update Observer with the new state
            input <- readIORef inputS
            let newObs@(Observer from dir) = applyInput input obs
            glClear $  GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

            lookAt' from dir
            act step win

            glFlush

            GLFW.pollEvents
            GLFW.swapBuffers win
            threadDelay 20000
            loop' (succ step) newObs

resizeWindow :: GLFW.WindowSizeCallback
resizeWindow _ w h =
  do
    glViewport 0 0 (fromIntegral w) (fromIntegral h)
    glMatrixMode GL_PROJECTION
    glLoadIdentity

-- | Loop gl initialization
initGL :: GLFW.Window -> IO ()
initGL win = do
  glClearColor 0 0 0 0
  glClear GL_COLOR_BUFFER_BIT
  glClearDepth 1 -- enables clearing of the depth buffer
  glEnable GL_DEPTH_TEST
  glDepthFunc GL_LEQUAL  -- type of depth test
  glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST
  (w,h) <- GLFW.getFramebufferSize win
  resizeScene win w h
  where
    resizeScene :: GLFW.FramebufferSizeCallback
    resizeScene win' w     0      = resizeScene win' w 1 -- prevent divide by zero
    resizeScene _   width height = do
      glViewport 0 0 (fromIntegral width) (fromIntegral height)
      glMatrixMode GL_PROJECTION
      glLoadIdentity
      gluPerspective 60 (fromIntegral width/fromIntegral height) 0.1 1000
      glMatrixMode GL_MODELVIEW
      glLoadIdentity
      glFlush

-- | Function given to glfw. On key input from glfw Input state (IORef) is updated
keyCallback :: IORef Input -> GLFW.Window -> GLFW.Key -> p1 -> GLFW.KeyState -> p2 -> IO ()
keyCallback _  win GLFW.Key'Escape _ GLFW.KeyState'Pressed  _ = shutdown win
keyCallback iS _   k               _ GLFW.KeyState'Pressed  _ = modifyIORef iS (pressKey $ readInputKey k)
keyCallback iS _   k               _ GLFW.KeyState'Released _ = modifyIORef iS (releaseKey $ readInputKey k)
keyCallback _  _   _               _ _                      _ = return ()

shutdown :: GLFW.Window -> IO ()
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  void exitSuccess
