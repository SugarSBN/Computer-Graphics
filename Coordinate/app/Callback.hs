module Callback where

import Graphics.UI.GLFW
import Graphics.GL
import Camera

process :: Window -> Camera -> IO ()
process window c = do
    keyState <- getKey window Key'Escape
    case keyState of
        KeyState'Pressed -> setWindowShouldClose window True
        _ -> pure ()

    keyState <- getKey window Key'W 
    case keyState of
        KeyState'Pressed -> moveCamera 0.05 Forward c
        _ -> pure ()

    keyState <- getKey window Key'S
    case keyState of
        KeyState'Pressed -> moveCamera 0.05 Backward c
        _ -> pure ()
    
    keyState <- getKey window Key'A
    case keyState of
        KeyState'Pressed -> moveCamera 0.05 Leftward c
        _ -> pure ()

    keyState <- getKey window Key'D
    case keyState of
        KeyState'Pressed -> moveCamera 0.05 Rightward c
        _ -> pure ()



frameBufferSizeCallback :: Window -> Int -> Int -> IO ()
frameBufferSizeCallback _ x y = glViewport 0 0 (fromIntegral x) (fromIntegral y)
  
