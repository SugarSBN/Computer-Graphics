
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Shader where

import qualified Data.Vector.Storable as V
import           Graphics.GL
import           Foreign
import           System.Exit
import           Control.Monad
import           Foreign.C

proxySizeOf :: forall a p. Storable a => p a -> Int
proxySizeOf _ = sizeOf (undefined :: a)

makeShaderProgram :: IO GLuint
makeShaderProgram =
    join $ createShaderProgram
    <$> compileVertexShader
    <*> compileFragmentShader

type VAO = Ptr GLuint
type VBO = Ptr GLuint

createShaderProgram :: GLuint -> GLuint -> IO GLuint
createShaderProgram vertexShader fragmentShader = do
  shaderProgram <- glCreateProgram
  glAttachShader shaderProgram vertexShader
  glAttachShader shaderProgram fragmentShader
  glLinkProgram shaderProgram
  checkShaderLinking shaderProgram
  glDeleteShader vertexShader
  glDeleteShader fragmentShader
  pure shaderProgram

checkShaderLinking :: GLuint -> IO ()
checkShaderLinking shaderProgram = do
  alloca $ \successPtr -> do
    alloca $ \infoLogPtr -> do
      glGetProgramiv shaderProgram GL_LINK_STATUS successPtr
      success <- peek successPtr
      if success <= 0
        then do
          glGetProgramInfoLog shaderProgram 512 nullPtr infoLogPtr
          putStrLn =<< peekCString infoLogPtr
          exitFailure
        else do
          putStrLn "Successfully linked shader program"

compileVertexShader :: IO GLuint
compileVertexShader = do
  putStrLn "Compiling vertex shader"
  vertexShader <- glCreateShader GL_VERTEX_SHADER
  vertexShaderSource <- newCString =<< getVertexShader
  alloca $ \shadersStr -> do
    shadersStr `poke` vertexShaderSource
    glShaderSource vertexShader 1 shadersStr nullPtr
    glCompileShader vertexShader
    checkShaderCompilation vertexShader
  pure vertexShader

getVertexShader :: IO String
getVertexShader = readFile "vert.glsl"

compileFragmentShader :: IO GLuint
compileFragmentShader = do
  putStrLn "Compiling fragment shader"
  fragmentShader <- glCreateShader GL_FRAGMENT_SHADER
  fragmentShaderSource <- newCString =<< getFragmentShader
  alloca $ \shadersStr -> do
    shadersStr `poke` fragmentShaderSource
    glShaderSource fragmentShader 1 shadersStr nullPtr
    glCompileShader fragmentShader
    checkShaderCompilation fragmentShader
  pure fragmentShader

getFragmentShader :: IO String
getFragmentShader = readFile "frag.glsl"

checkShaderCompilation :: GLuint -> IO ()
checkShaderCompilation shader = do
  alloca $ \successPtr ->
    alloca $ \infoLogPtr -> do
      glGetShaderiv shader GL_COMPILE_STATUS successPtr
      success <- peek successPtr
      glGetShaderInfoLog shader 512 nullPtr infoLogPtr
      mapM_ print . lines =<< peekCString infoLogPtr
      when (success <= 0) $ do
        putStrLn "Failed to compile shader "
        exitFailure
      putStrLn "Compiled shader successfully"
