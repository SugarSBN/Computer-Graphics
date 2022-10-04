module Callback where

import Graphics.UI.GLFW
import Graphics.GL
import Camera
import Data.Fixed
import Data.IORef (modifyIORef, IORef, writeIORef, readIORef)
import Model
import Linear

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
  
cursorPosCallback :: (IORef Float, IORef Float) -> (IORef Float, IORef Float) -> Camera -> IORef [Model] -> IORef [Int] -> Window -> Double -> Double -> IO ()
cursorPosCallback (lastX, lastY) (yaw, pitch) c mds selected window x y = do
    lastX' <- readIORef lastX
    lastY' <- readIORef lastY
    let xoffset = realToFrac $ (x - realToFrac lastX') * 0.05
    let yoffset = realToFrac $ (realToFrac lastY' - y) * 0.05
    writeIORef lastX (realToFrac x)    
    writeIORef lastY (realToFrac y)
    modifyIORef yaw (+xoffset)
    modifyIORef pitch (\s -> if s + yoffset > 89.0 then 89.9 else (if s + yoffset < -89.9 then -89.0 else s + yoffset))
    y' <- readIORef yaw
    p' <- readIORef pitch
    let y = y' / 180.0 * pi
    let p = p' / 180.0 * pi
    writeIORef (cameraFront c) (V3 (cos y * cos p) (sin p) (sin y * cos p))

    pos <- readIORef (cameraPos c)
    front <- readIORef (cameraFront c)
    mds' <- readIORef mds
    let inter = filter (\x -> interModelLine (mds' !! x) pos front) [0 .. (length mds' - 1)]
    let inter' = filter (\x -> not (interModelLine (mds' !! x) pos front)) [0 .. (length mds' - 1)]
    selected' <- readIORef selected
    mapM_ (\x -> do
                    writeIORef (head (modelColor (mds' !! x))) (V4 0.0 1.0 1.0 1.0))
        inter
    mapM_ (\x -> do
                    writeIORef (head (modelColor (mds' !! x))) (V4 1.0 1.0 1.0 1.0))
        inter'
    mapM_ (\x -> do
                    writeIORef (head (modelColor (mds' !! x))) (V4 1.0 0.0 0.0 1.0))
        selected'

mouseCallback :: Camera -> IORef [Model] -> IORef [Int] -> Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mouseCallback c mds selected window MouseButton'1 MouseButtonState'Pressed _ = do
    pos <- readIORef (cameraPos c)
    front <- readIORef (cameraFront c)
    mds' <- readIORef mds
    let modelIndex = filter (\x -> interModelLine (mds' !! x) pos front) [0 .. (length mds' - 1)]
    writeIORef selected modelIndex

mouseCallback c mds selected window MouseButton'2 MouseButtonState'Pressed _ = do
    mds' <- readIORef mds
    writeIORef selected []
mouseCallback _ _ _ _ _ _ _ = return ()

scrollCallback :: Camera -> Window -> Double -> Double -> IO ()
scrollCallback c window _ yoffset = do
    aspect <- readIORef (cameraAspect c)
    let aspect' = aspect - yoffset
    writeIORef (cameraAspect c) (max (min aspect' 45.0) 1.0)
