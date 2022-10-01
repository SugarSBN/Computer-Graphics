module Model where

import ReadParse
import Graphics.GL
import Data.IORef
import Linear

data Model = Model {
    vertices :: [[GLfloat]],
    nsurfaces :: GLint,
    position :: IORef (V3 GLfloat)
}

readModel :: FilePath -> IO Model
readModel filename = do
    verts <- readVertices filename
    let mx = maximum (map abs (concat verts))
    pos <- newIORef (V3 0.0 0.0 0.0)
    return $ Model (map (map (/ mx)) verts) (fromIntegral (length verts)) pos

combineModel :: Model -> Model ->IO  Model
combineModel m1 m2 = do
    let v1 = vertices m1
    let v2 = vertices m2
    (V3 p1x p1y p1z) <- readIORef (position m1)
    (V3 p2x p2y p2z) <- readIORef (position m2)
    let deltaX = p2x - p1x
    let deltaY = p2y - p1y
    let deltaZ = p2z - p1z
    let v2' = map (\s -> [head s + deltaX, (s !! 1) + deltaY, (s !! 2) + deltaZ, 
                         (s !! 3) + deltaX, (s !! 4) + deltaY, (s !! 5) + deltaZ,
                         (s !! 6) + deltaX, (s !! 7) + deltaY, (s !! 8) + deltaZ]) v2
    return $ Model (v1 ++ v2') (nsurfaces m1 + nsurfaces m2) (position m1)
    
