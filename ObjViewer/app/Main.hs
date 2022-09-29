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
import           ReadParse

import Callback
import Shader
import ReadParse

main :: IO ()
main = do
  init
  windowHint (WindowHint'ContextVersionMajor 3)
  windowHint (WindowHint'ContextVersionMinor 3)
  windowHint (WindowHint'OpenGLProfile OpenGLProfile'Core)
  windowHint (WindowHint'OpenGLForwardCompat True)
  maybeWindow <- createWindow 800 600 "Learn OpenGL" Nothing Nothing
  case maybeWindow of
    Nothing -> do
      putStrLn "Failed to create GLFW window"
      terminate
      exitFailure
    Just window -> do
      makeContextCurrent (Just window)
      setFramebufferSizeCallback window (Just frameBufferSizeCallback)
      shaderProgram <- makeShaderProgram
      initBuffers $ \vaoPtr vboPtr ->
        forever $ do
          shouldClose <- windowShouldClose window
          if shouldClose
            then do
              glDeleteProgram shaderProgram
              terminate
              exitSuccess
            else do
              process window
              render shaderProgram vaoPtr window
              swapBuffers window
              pollEvents


g :: IO (M44 Float)
g = do
    t <- getTime 
    let theta = case t of
                    Nothing -> 0
                    Just p  -> mod' p (2 * pi)
    return $ mkTransformation 
                (axisAngle (V3 0 0.5 0.0) (realToFrac theta))
                (V3 0.0 0.0  0.0)


initBuffers :: (VBO -> VAO -> IO ()) -> IO ()
initBuffers f = do
  alloca $ \vaoPtr -> do
   alloca $ \vboPtr -> do
    glGenVertexArrays 1 vaoPtr
    glGenBuffers 1 vboPtr
    peek vaoPtr >>= glBindVertexArray
    peek vboPtr >>= glBindBuffer GL_ARRAY_BUFFER
    verts <- verts'
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



verts' :: IO (Vector Float)
verts' = do
    vertices <- readVertices "cat.obj"
    print (length vertices)
    return $ V.fromList vertices

render :: GLuint -> Ptr GLuint -> Window -> IO ()
render shaderProgram vaoPtr window = do
  vao <- peek vaoPtr
  glClearColor 0.2 0.3 0.3 1.0
  glClear GL_COLOR_BUFFER_BIT
  glUseProgram shaderProgram
  g' <- g
  setMatrix shaderProgram "transform" g'
  glBindVertexArray vao
  let sts = [i * 3 | i <- [0 .. 2081]]
  mapM_ (\x -> glDrawArrays GL_LINE_LOOP x 3) sts
  

setMatrix :: GLuint -> String -> M44 Float -> IO ()
setMatrix id' name val = do
    withCString name $ \cstr -> do
        location <- glGetUniformLocation id' cstr
        withPtr val (glUniformMatrix4fv location 1 GL_TRUE)

withPtr :: (Storable a, Floating a, Foldable f, Foldable g) => f (g a) -> (Ptr a -> IO ()) -> IO ()
withPtr = V.unsafeWith . V.fromList . concatMap F.toList
