module Main where

import           Prelude              hiding (init)
import           Graphics.UI.GLFW
import qualified Data.Vector.Storable as V
import qualified Data.Foldable        as F
import           Data.Vector.Storable (Vector)
import           Graphics.GL
import           Foreign
import           System.Exit
import           Data.Fixed
import           Foreign.C
import           Control.Monad
import           Linear

import Callback
import Shader
import Camera
import ReadParse
import Model
import Graphics.Rendering.OpenGL (shaderCompiler)
import Graphics.GL.Compatibility32 
import Data.IORef (readIORef, newIORef)

main :: IO ()
main = do
  init
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  windowHint (WindowHint'OpenGLForwardCompat True)
  maybeWindow <- createWindow 800 800 "Learn OpenGL" Nothing Nothing
  case maybeWindow of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      terminate
      exitFailure
    Just window -> do
      makeContextCurrent (Just window)
      setFramebufferSizeCallback window (Just frameBufferSizeCallback)
      shaderProgram <- makeShaderProgram
      glEnable GL_BLEND
      setCursorInputMode window CursorInputMode'Disabled 
       
      pos <- newIORef (V3 0.0 0.0 3.0)
      front <- newIORef (V3 0.0 0.0 (-1.0))
      up <- newIORef (V3 0.0 1.0 0.0)
    
      yaw <- newIORef (-90.0)
      pitch <- newIORef 0.0

      lastX <- newIORef 400.0
      lastY <- newIORef 400.0
      
      m <- readModel "cat.obj"
      mds <- newIORef [m]

      --glEnable GL_DEPTH_TEST
      setCursorPosCallback window (Just (cursorPosCallback (lastX, lastY) (yaw, pitch) (Camera pos front up)))
      setMouseButtonCallback window (Just (mouseCallback m (Camera pos front up)))
      forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram
              terminate
              exitSuccess
            else do
              process window (Camera pos front up)
              mds' <- readIORef mds
              m' <- foldM combineModel (head mds') (tail mds')
              initBuffers m' $ \vaoPtr vboPtr->
                  render m' shaderProgram (Camera pos front up) vaoPtr window

              swapBuffers window
              pollEvents


initBuffers :: Model -> (VBO -> VAO -> IO ()) -> IO ()
initBuffers m f = do
  alloca $ \vaoPtr -> do
   alloca $ \vboPtr -> do
    glGenVertexArrays 1 vaoPtr
    glGenBuffers 1 vboPtr
    peek vaoPtr >>= glBindVertexArray
    peek vboPtr >>= glBindBuffer GL_ARRAY_BUFFER
    let verts = V.fromList $ concat (vertices m)
    V.unsafeWith verts $ \vertsPtr ->
      glBufferData
        GL_ARRAY_BUFFER
        (fromIntegral (V.length verts * proxySizeOf verts))
        (castPtr vertsPtr)
        GL_STATIC_DRAW
    glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE
      (fromIntegral (sizeOf (undefined :: Float) * 3))
        nullPtr
    glEnableVertexAttribArray 0
    glBindBuffer GL_ARRAY_BUFFER 0
    glBindVertexArray 0
    f vaoPtr vboPtr

model :: Model ->  IO (M44 Float)
model m = do
    tims <- getTime 
    let theta = case tims of
                  Nothing -> 0
                  Just s  -> mod' s (2 * pi)
    pos <- readIORef (position m)
    return $ mkTransformation (axisAngle (V3 0.0 1.0 0.0) (realToFrac theta)) pos

view :: Camera -> IO (M44 Float)
view c = do
    pos <- readIORef (cameraPos c)
    front <- readIORef (cameraFront c)
    up <- readIORef (cameraUp c)
    return $ lookAt pos (pos + front) up

projection :: M44 Float
projection = perspective 45.0 1.0 0.1 100.0

render :: Model -> GLuint -> Camera -> Ptr GLuint -> Window -> IO ()
render m shaderProgram c vaoPtr window = do
  vao <- peek vaoPtr
  glClearColor 0.2 0.3 0.3 1.0
  --glClearDepth 1
  --glDepthMask GL_TRUE
  --glEnable GL_DEPTH_TEST
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  glUseProgram shaderProgram
  model' <- model m
  setMatrix shaderProgram "model" model'
  view' <- view c
  setMatrix shaderProgram "view" view'
  setMatrix shaderProgram "projection" projection
  glBindVertexArray vao
  let sts = [i * 3 | i <- [0 .. (nsurfaces m - 1)]]
  mapM_ (\x -> glDrawArrays GL_LINE_LOOP x 3) sts

  

setMatrix :: GLuint -> String -> M44 Float -> IO ()
setMatrix id' name val = do
    withCString name $ \cstr -> do
        location <- glGetUniformLocation id' cstr
        withPtr val (glUniformMatrix4fv location 1 GL_TRUE)

withPtr :: (Storable a, Floating a, Foldable f, Foldable g) => f (g a) -> (Ptr a -> IO ()) -> IO ()
withPtr = V.unsafeWith . V.fromList . concatMap F.toList
