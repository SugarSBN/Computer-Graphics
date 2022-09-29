module Callback where

import Graphics.UI.GLFW
import Graphics.GL

process :: Window -> IO ()
process window = do
    keyState <- getKey window Key'Escape
    case keyState of
        KeyState'Pressed -> setWindowShouldClose window True
        _ -> pure ()

frameBufferSizeCallback :: Window -> Int -> Int -> IO ()
frameBufferSizeCallback _ x y = glViewport 0 0 (fromIntegral x) (fromIntegral y)
  
