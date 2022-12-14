module Callback where

import Graphics.UI.GLFW
import Graphics.GL
import Camera
import Data.Fixed
import Data.IORef (modifyIORef, IORef, writeIORef, readIORef)
import Model
import Bezier
import Control.Monad
import Linear

data MousePosition = MousePosition {
    positionX :: Double,
    positionY :: Double
}

process :: IORef Float ->
           IORef Bool ->
           IORef [Model] ->
           IORef [Int] ->
           IORef [Model] -> 
           Window -> IORef Camera -> IO ()
process totTime' anime' mds' selected' beziers' window c = do
    keyState <- getKey window Key'Escape
    case keyState of
        KeyState'Pressed -> setWindowShouldClose window True
        _ -> pure ()

    keyState <- getKey window Key'W 
    case keyState of
        KeyState'Pressed -> modifyIORef c (\x -> moveCamera x 0.05 Forward)
        _ -> pure ()

    keyState <- getKey window Key'S
    case keyState of
        KeyState'Pressed -> modifyIORef c (\x -> moveCamera x 0.05 Backward)
        _ -> pure ()
    
    keyState <- getKey window Key'A
    case keyState of
        KeyState'Pressed -> modifyIORef c (\x -> moveCamera x 0.05 Leftward)
        _ -> pure ()

    keyState <- getKey window Key'D
    case keyState of
        KeyState'Pressed -> modifyIORef c (\x -> moveCamera x 0.05 Rightward)
        _ -> pure ()
    
    mds <- readIORef mds'
    selected <- readIORef selected'

    keyState <- getKey window Key'Left 
    case keyState of
        KeyState'Pressed -> 
            if null selected 
               then pure ()
               else do
                let id = head selected
                modifyIORef mds' (\l ->
                    modifyList l id (\m ->
                        rotateModel m (axisAngle (V3 0.0 1.0 0.0) 0.05)))
        _ -> pure ()
    
    keyState <- getKey window Key'Right 
    case keyState of
        KeyState'Pressed ->
            if null selected
               then pure ()
               else do
                let id = head selected
                modifyIORef mds' (\l ->
                    modifyList l id (\m ->
                        rotateModel m (axisAngle (V3 0.0 1.0 0.0) (-0.05))))
        _ -> pure ()

    keyState <- getKey window Key'Up
    case keyState of
        KeyState'Pressed ->
            if null selected
               then pure ()
               else do
                let id = head selected
                modifyIORef mds' (\l ->
                    modifyList l id (\m ->
                        rotateModel m (axisAngle (V3 1.0 0.0 0.0) 0.05)))
        _ -> pure ()

    keyState <- getKey window Key'Down 
    case keyState of
       KeyState'Pressed ->
           if null selected
              then pure ()
              else do
               let id = head selected
               modifyIORef mds' (\l ->
                   modifyList l id (\m ->
                       rotateModel m (axisAngle (V3 1.0 0.0 0.0) (-0.05))))
       _ -> pure ()

    keyState <- getKey window Key'Space 
    case keyState of
      KeyState'Pressed -> do
              anime <- readIORef anime'
              if not anime
                 then do
                     totTime <- readIORef totTime'                     
                     writeIORef beziers' (bezier totTime mds)
                 else writeIORef beziers' mds
              writeIORef anime' (not anime)
      _ -> pure () 

    keyState <- getKey window Key'Q 
    case keyState of
      KeyState'Pressed -> do
            anime <- readIORef anime'
            if anime then do
                modifyIORef totTime' (\s -> s - 10.0)
                totTime <- readIORef totTime'
                writeIORef beziers' (bezier totTime mds)
            else pure ()
      _ -> pure ()

    keyState <- getKey window Key'E
    case keyState of
      KeyState'Pressed -> do
            anime <- readIORef anime'
            if anime then do
                modifyIORef totTime' (\s -> s + 10.0)
                totTime <- readIORef totTime'
                writeIORef beziers' (bezier totTime mds)
            else pure ()
      _ -> pure ()

frameBufferSizeCallback :: Window -> Int -> Int -> IO ()
frameBufferSizeCallback _ x y = glViewport 0 0 (fromIntegral x) (fromIntegral y)
  
cursorPosCallback :: IORef MousePosition -> 
                     IORef Camera -> 
                     IORef [Model] -> 
                     IORef [Int] -> 
                     Window -> Double -> Double -> IO ()
cursorPosCallback lastPosition' c' mds' selected' window x y = do
    lastPos <- readIORef lastPosition'
    let lastX = positionX lastPos
    let lastY = positionY lastPos
    let xoffset = realToFrac $ (x - realToFrac lastX) * 0.05
    let yoffset = realToFrac $ (realToFrac lastY - y) * 0.05
    
    writeIORef lastPosition' (MousePosition x y)

    modifyIORef c' (\c -> modifyYaw c (+xoffset))
    modifyIORef c' (\c -> modifyPitch c (\s -> if s + yoffset > 89.0 then 89.9 else (if s + yoffset < -89.9 then -89.0 else s + yoffset)))
    

    c <- readIORef c'
    let y' = yaw c
    let p' = pitch c
    let y = y' / 180.0 * pi
    let p = p' / 180.0 * pi
    modifyIORef c' (\c -> modifyFront c (const (V3 (realToFrac (cos y * cos p)) (realToFrac (sin p)) (realToFrac (sin y * cos p)))))
    
    let pos = cameraPos c
    let front = cameraFront c
    mds <- readIORef mds'
    let inter = filter (\x -> interModelLine (mds !! x) pos front) [0 .. (length mds - 1)]
    let notInter = filter (\x -> not (interModelLine (mds !! x) pos front)) [0 .. (length mds - 1)]
    selected <- readIORef selected'

    if not (null inter) then do
        modifyIORef mds' (\l -> 
                modifyList l (head inter) (\m -> modifyColor m (const (V4 0.0 1.0 1.0 1.0))))
        else pure ()
    
    if not (null notInter) then do
        mapM_ (\ind ->
                modifyIORef mds' (\l ->
                    modifyList l (notInter !! ind) (\m -> modifyColor m (const (V4 1.0 1.0 1.0 1.0))))
              ) [0 .. (length notInter - 1)]
        else pure ()

    if not (null selected) then do
        modifyIORef mds' (\l ->
                modifyList l (head selected) (\m -> modifyColor m (const (V4 1.0 0.0 0.0 1.0))))
        else pure ()

mouseCallback :: IORef Camera -> 
                 IORef [Model] -> 
                 IORef [Int] -> 
                 Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
mouseCallback c' mds' selected' window MouseButton'1 MouseButtonState'Pressed _ = do
    c <- readIORef c'
    let pos = cameraPos c
    let front = cameraFront c

    mds <- readIORef mds'
    selected <- readIORef selected'
    unless (null selected) 
        $ do
            let rua = mds !! head selected
            let whe = position rua
            let m' = translateModel rua (pos - whe + 5 *^ front) 
            modifyIORef mds' (++ [m'])

    let modelIndex = filter (\x -> interModelLine (mds !! x) pos front) [0 .. (length mds - 1)]
    writeIORef selected' modelIndex
    
    selected <- readIORef selected'
    if not (null selected) then do
        modifyIORef mds' (\l ->
            modifyList l (head selected) (\m -> modifyColor m (const (V4 1.0 0.0 0.0 1.0))))
        else pure ()

mouseCallback c' mds' selected' window MouseButton'2 MouseButtonState'Pressed _ = do
    selected <- readIORef selected'
    if not (null selected) 
       then do
            writeIORef selected' []
       else do
            mds <- readIORef mds'
            modifyIORef mds' (take (length mds - 1))
mouseCallback _ _ _ _ _ _ _ = return ()

scrollCallback :: IORef [Model] ->
                  IORef Camera -> 
                  IORef [Int] ->
                  Window -> Double -> Double -> IO ()
scrollCallback mds' c' selected' window _ yoffset = do
    c <- readIORef c'
    selected <- readIORef selected'
    mds <- readIORef mds'
    if null selected 
        then do
            let aspect = cameraAspect c
            let aspect' = aspect - yoffset
            modifyIORef c' (\c -> modifyAspect c (const (max (min aspect' 45.0) 1.0)))
        else do
            let id = head selected
            modifyIORef mds' (\l ->
                modifyList l id (enlargeModel (0.1 * realToFrac yoffset)))

keyCallback :: Window -> Key -> Int -> KeyState -> ModifierKeys -> IO ()

keyCallback _ _ _ _ _ = return ()
