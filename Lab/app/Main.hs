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
import Graphics.Rendering.OpenGL (shaderCompiler, shaderBinary, shaderPrecisionFormat, shadeModel)
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
       
      camera' <- newIORef (
                    Camera
                        (V3 0.0 0.0 3.0)
                        (V3 0.0 0.0 (-1.0))
                        (V3 0.0 1.0 0.0)
                        45.0
                        (-90.0)
                        0.0) 
       
      mCat <- readModel "cat.obj"
      let mCoordinate = Model
                            [[-10.0, 0, 0, 10, 0, 0, -10, 0, 0], 
                              [0, -10, 0, 0, 10, 0, 0, -10, 0], 
                              [0, 0, -10, 0, 0, 10, 0, 0, -10], 
                              [-1, 0, -1, -1, 0, 1, -1, 0, -1], 
                              [-1, 0, 1, 1, 0, 1, -1, 0, 1], 
                              [1, 0, 1, 1, 0, -1, 1, 0, -1], 
                              [1, 0, -1, -1, 0, -1, 1, 0, -1]] 
                              7 
                              (V3 0.0 0.0 0.0)
                              (V4 1.0 1.0 1.0 1.0)
                              0
                              0
      
      allModels' <- newIORef [mCoordinate, mCat]
      selectedModel' <- newIORef []
     

      mouseLastPosition' <- newIORef (MousePosition 400.0 400.0)

      --glEnable GL_DEPTH_TEST
      setCursorPosCallback window (Just (cursorPosCallback mouseLastPosition' camera' allModels' selectedModel'))
      setMouseButtonCallback window (Just (mouseCallback camera' allModels' selectedModel'))
      setScrollCallback window (Just (scrollCallback allModels' camera' selectedModel'))
      setKeyCallback window (Just keyCallback)
      forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram
              terminate
              exitSuccess
            else do
              process allModels' selectedModel' window camera'
              mds <- readIORef allModels'
              
              let m = packModels mds
              camera <- readIORef camera'
              initBuffers m $ \vaoPtr vboPtr->
                  render mds shaderProgram camera vaoPtr window

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

model :: Model -> M44 Float
model m = mkTransformation (axisAngle (V3 0.0 1.0 0.0) 0.0) (position m)

view :: Camera -> M44 Float
view c = lookAt pos (pos + front) up
    where
        pos = cameraPos c
        front = cameraFront c
        up = cameraUp c

projection :: Camera -> M44 Float
projection c = perspective (realToFrac aspect / 180.0 * pi) 1.0 0.1 100.0
    where
        aspect = cameraAspect c

render :: [Model] -> GLuint -> Camera -> Ptr GLuint -> Window -> IO ()
render mds shaderProgram c vaoPtr window = do
  vao <- peek vaoPtr
  glClearColor 0.2 0.3 0.3 1.0
  --glClearDepth 1
  --glDepthMask GL_TRUE
  --glEnable GL_DEPTH_TEST
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)
  
  glUseProgram shaderProgram
  
  let m = packModels mds
  let vertices = packVertices mds
  let colors = packColors mds

  setMatrix shaderProgram "model" (model m)
  setMatrix shaderProgram "view" (view c)
  setMatrix shaderProgram "projection" (projection c)
  glBindVertexArray vao
  let n = length vertices
  mapM_ (\ind -> do
                    let col = colors !! ind
                    setVector4 shaderProgram "outColor" col
                    mapM_ (\x -> glDrawArrays GL_LINE_LOOP x 3) (vertices !! ind)
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
