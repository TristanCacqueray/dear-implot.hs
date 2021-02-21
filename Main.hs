{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import DearImGui
import DearImGui.OpenGL2
import qualified DearImGui.Plot as ImPlot
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import Graphics.GL
import SDL

main :: IO ()
main = do
  -- Initialize SDL
  initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    win <- do
      let title = "Hello, Dear ImGui!"
      let config = defaultWindow {windowGraphicsContext = OpenGLContext defaultOpenGL}
      managed $ bracket (createWindow title config) destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket (glCreateContext win) glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket createContext destroyContext

    -- Create an ImPlot context
    _ <- managed $ bracket ImPlot.createContext ImPlot.destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ (sdl2InitForOpenGL win glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    liftIO $ mainLoop win

mainLoop :: Window -> IO ()
mainLoop win = do
  -- Process the event loop
  untilNothingM pollEventWithImGui

  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame win
  newFrame

  -- Build the GUI
  bracket_ (ImPlot.beginPlot "Hello, ImPlot!") ImPlot.endPlot do
    ImPlot.plotLine "test" [0.0, 0.1, 0.2, 0.3, 0.4] [0.1, 0.2, 0.3, 0.1, 0.5]

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  render
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow win

  mainLoop win
  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)
