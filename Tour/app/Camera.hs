module Camera where

import Data.IORef
import Linear
import Graphics.GL

data Camera = Camera {
    cameraPos :: IORef (V3 Float),
    cameraFront :: IORef (V3 Float),
    cameraUp  :: IORef (V3 Float),
    cameraAspect :: IORef Double
}

data CameraDirection = Forward | Backward | Leftward | Rightward

moveCamera :: GLfloat -> CameraDirection -> Camera -> IO ()
moveCamera speed dire c = do
    pos <- readIORef (cameraPos c)
    front <- readIORef (cameraFront c)
    up <- readIORef (cameraUp c)
    case dire of
        Forward   -> modifyIORef (cameraPos c) $ \v -> v + (speed *^ front) 
        Backward  -> modifyIORef (cameraPos c) $ \v -> v - (speed *^ front)
        Leftward  -> modifyIORef (cameraPos c) $ \v -> v - (speed *^ normalize (cross front up))
        Rightward -> modifyIORef (cameraPos c) $ \v -> v + (speed *^ normalize (cross front up))
