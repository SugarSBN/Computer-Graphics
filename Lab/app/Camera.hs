module Camera where

import Data.IORef
import Linear
import Graphics.GL

data Camera = Camera {
    cameraPos    :: V3 Float,
    cameraFront  :: V3 Float,
    cameraUp     :: V3 Float,
    cameraAspect :: Double,
    yaw          :: Double,
    pitch        :: Double
}

data CameraDirection = Forward | Backward | Leftward | Rightward

moveCamera :: Camera -> GLfloat -> CameraDirection -> Camera
moveCamera c speed dire = 
        case dire of
          Forward   -> Camera (pos + (speed *^ front)) front up asp y p
          Backward  -> Camera (pos - (speed *^ front)) front up asp y p
          Leftward  -> Camera (pos - (speed *^ normalize (cross front up))) front up asp y p 
          Rightward -> Camera (pos + (speed *^ normalize (cross front up))) front up asp y p
        where
            pos = cameraPos c
            front = cameraFront c
            up = cameraUp c
            asp = cameraAspect c
            y = yaw c
            p = pitch c

modifyYaw :: Camera -> (Double -> Double) -> Camera
modifyYaw c f = Camera
                    (cameraPos c)
                    (cameraFront c)
                    (cameraUp c)
                    (cameraAspect c)
                    (f (yaw c))
                    (pitch c)

modifyPitch :: Camera -> (Double -> Double) -> Camera
modifyPitch c f = Camera
                    (cameraPos c)
                    (cameraFront c)
                    (cameraUp c)
                    (cameraAspect c)
                    (yaw c)
                    (f (pitch c))

modifyFront :: Camera -> (V3 Float -> V3 Float) -> Camera
modifyFront c f = Camera
                    (cameraPos c)
                    (f (cameraFront c))
                    (cameraUp c)
                    (cameraAspect c)
                    (yaw c)
                    (pitch c)

modifyAspect :: Camera -> (Double -> Double) -> Camera
modifyAspect c f = Camera
                    (cameraPos c)
                    (cameraFront c)
                    (cameraUp c)
                    (f (cameraAspect c))
                    (yaw c)
                    (pitch c)
