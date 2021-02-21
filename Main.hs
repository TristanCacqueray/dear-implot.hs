{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Managed
import Data.Binary.Get (getInt16le, isEmpty, runGet)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromStrict)
import Data.List (iterate')
import DearImGui
import DearImGui.OpenGL2
import qualified DearImGui.Plot as ImPlot
import DearImGui.SDL
import DearImGui.SDL.OpenGL
import GHC.Float (int2Float)
import GHC.Int (Int16)
import Graphics.GL
import Pipes
import Pipes.PulseSimple
import Pipes.Safe (runSafeT)
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

    liftIO $ runSafeT (runEffect (readPulse "dear-pulse" Nothing 25 >-> mainLoop win))

-- | Binary decoder
decodeSampleList :: BS.ByteString -> [Int16]
decodeSampleList = runGet get . fromStrict
  where
    get = do
      empty <- isEmpty
      if empty
        then return []
        else do
          sample <- getInt16le
          rest <- get
          return (sample : rest)

mainLoop :: MonadIO m => Window -> Consumer' BS.ByteString m ()
mainLoop win = do
  -- Process the event loop
  untilNothingM pollEventWithImGui

  -- Get audio buffer
  buf <- await
  let maxInt16 :: Int16
      maxInt16 = maxBound
      maxInt16f = int2Float $ fromIntegral maxInt16
      samples :: [Float]
      samples = map (\x' -> int2Float (fromIntegral x') / maxInt16f) $ decodeSampleList buf

  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame win
  newFrame

  -- Build the GUI
  ImPlot.setNextPlotLimits (0, 1) (-1, 1)
  liftIO $ bracket_ (ImPlot.beginPlot "Audio") ImPlot.endPlot do
    ImPlot.plotLine "pulse-input" xs samples

  -- Render
  glClear GL_COLOR_BUFFER_BIT

  render
  openGL2RenderDrawData =<< getDrawData

  glSwapWindow win

  mainLoop win
  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)
    xs = range
    range :: [Float]
    range = take 1764 $ iterate' (+ step) 0.0
    step :: Float
    step = 1 / 1764
