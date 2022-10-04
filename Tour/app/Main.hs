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
import Graphics.Rendering.OpenGL (shaderCompiler, shaderBinary, shaderPrecisionFormat)
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
      aspect <- newIORef 45.0
    
      yaw <- newIORef (-90.0)
      pitch <- newIORef 0.0

      lastX <- newIORef 400.0
      lastY <- newIORef 400.0
      
      m <- readModel "cat.obj"
      col <- newIORef (V4 1.0 1.0 1.0 1.0)
      mds <- newIORef [Model [[-10.0, 0, 0, 10, 0, 0, -10, 0, 0], 
                              [0, -10, 0, 0, 10, 0, 0, -10, 0], 
                              [0, 0, -10, 0, 0, 10, 0, 0, -10], 
                              [-1, 0, -1, -1, 0, 1, -1, 0, -1], 
                              [-1, 0, 1, 1, 0, 1, -1, 0, 1], 
                              [1, 0, 1, 1, 0, -1, 1, 0, -1], 
                              [1, 0, -1, -1, 0, -1, 1, 0, -1]] 7 (V3 0 0 0) [[0, 3, 6, 9, 12, 15, 18]] [col], m]
      selectedModel <- newIORef []

      --glEnable GL_DEPTH_TEST
      setCursorPosCallback window (Just (cursorPosCallback (lastX, lastY) (yaw, pitch) (Camera pos front up aspect) mds selectedModel))
      setMouseButtonCallback window (Just (mouseCallback (Camera pos front up aspect) mds selectedModel))
      setScrollCallback window (Just (scrollCallback (Camera pos front up aspect)))
      setKeyCallback window (Just keyCallback)
      forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram
              terminate
              exitSuccess
            else do
              process window (Camera pos front up aspect)
              mds' <- readIORef mds
              
              m' <- foldM combineModel (head mds') (tail mds')
              initBuffers m' $ \vaoPtr vboPtr->
                  render m' shaderProgram (Camera pos front up aspect) vaoPtr window

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
    return $ mkTransformation (axisAngle (V3 0.0 1.0 0.0) 0) (position m)

view :: Camera -> IO (M44 Float)
view c = do
    pos <- readIORef (cameraPos c)
    front <- readIORef (cameraFront c)
    up <- readIORef (cameraUp c)
    return $ lookAt pos (pos + front) up

projection :: Camera -> IO (M44 Float)
projection c = do
    aspect <- readIORef (cameraAspect c)
    return $ perspective (realToFrac aspect / 180.0 * pi) 1.0 0.1 100.0

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
  projection' <- projection c
  setMatrix shaderProgram "projection" projection'
  glBindVertexArray vao
  let n = length (verticeIndex m)
  mapM_ (\ind -> do
                    col <- readIORef (modelColor m !! ind)
                    setVector4 shaderProgram "outColor" col
                    mapM_ (\x -> glDrawArrays GL_LINE_LOOP x 3) (verticeIndex m !! ind)
        ) [0 .. (n - 1)]

  

setMatrix :: GLuint -> String -> M44 Float -> IO ()
setMatrix id' name val = do
    withCString name $ \cstr -> do
        location <- glGetUniformLocation id' cstr
        withPtr val (glUniformMatrix4fv location 1 GL_TRUE)

setVector4 :: GLuint -> String -> V4 Float -> IO ()
setVector4 id' name (V4 a b c d) = do
    withCString name $ \cstr -> do
        location <- glGetUniformLocation id' cstr
        glUniform4f location a b c d

withPtr :: (Storable a, Floating a, Foldable f, Foldable g) => f (g a) -> (Ptr a -> IO ()) -> IO ()
withPtr = V.unsafeWith . V.fromList . concatMap F.toList
