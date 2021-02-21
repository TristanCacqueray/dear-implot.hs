{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | DearImGui bindings for https://github.com/epezent/implot
module DearImGui.Plot where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as Cpp

C.context (Cpp.cppCtx <> C.bsCtx)
C.include "implot.h"
Cpp.using "namespace ImPlot"

newtype Context = Context (Ptr ())

createContext :: MonadIO m => m Context
createContext = liftIO do
  Context <$> [C.exp| void* { CreateContext() } |]

destroyContext :: MonadIO m => Context -> m ()
destroyContext (Context contextPtr) = liftIO do
  [C.exp| void { DestroyContext((ImPlotContext*)$(void* contextPtr)); } |]

beginPlot :: MonadIO m => String -> m Bool
beginPlot name = liftIO do
  withCString name \namePtr ->
    (0 /=) <$> [C.exp| bool { BeginPlot($(char* namePtr)) } |]

endPlot :: MonadIO m => m ()
endPlot = liftIO do
  [C.exp| void { EndPlot(); } |]

plotLine :: (MonadIO m) => String -> [Float] -> [Float] -> m ()
plotLine desc xs ys = liftIO $ do
  let size = fromIntegral $ length xs
  withCString desc \descPtr -> do
    withArray (map realToFrac xs) \xsPtr -> do
      withArray (map realToFrac ys) \ysPtr -> do
        [C.exp| void { PlotLine( $(char* descPtr), $(float *xsPtr), $(float *ysPtr), $(int size) ) } |]
