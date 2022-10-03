module Model where

import ReadParse
import Graphics.GL
import Data.IORef
import Linear

data Model = Model {
    vertices :: [[GLfloat]],
    nsurfaces :: GLint,
    position :: V3 GLfloat,
    verticeIndex :: [[GLint]],
    modelColor :: [IORef (V4 GLfloat)]
}

readModel :: FilePath -> IO Model
readModel filename = do
    verts <- readVertices filename
    let mx = maximum (map abs (concat verts))
    col <- newIORef (V4 1.0 1.0 1.0 1.0)
    return $ Model (map (map (/ mx)) verts) (fromIntegral (length verts)) (V3 0.0 0.0 0.0) [[fromIntegral (i * 3) | i <- [0 .. (length verts - 1)]]] [col]

translateModel :: Model -> V3 GLfloat -> Model
translateModel m v = Model (vertices m) (nsurfaces m) (position m + v) (verticeIndex m) (modelColor m)

combineModel :: Model -> Model ->IO  Model
combineModel m1 m2 = do
    let v1 = vertices m1
    let v2 = vertices m2
    let (V3 p1x p1y p1z) = position m1
    let (V3 p2x p2y p2z) = position m2
    let deltaX = p2x - p1x
    let deltaY = p2y - p1y
    let deltaZ = p2z - p1z
    let v2' = map (\s -> [head s + deltaX, (s !! 1) + deltaY, (s !! 2) + deltaZ, 
                         (s !! 3) + deltaX, (s !! 4) + deltaY, (s !! 5) + deltaZ,
                         (s !! 6) + deltaX, (s !! 7) + deltaY, (s !! 8) + deltaZ]) v2
    let verticeIndex2 = verticeIndex m2
    let verticeIndex2' = map (map (+(3 * nsurfaces m1))) verticeIndex2
    return $ Model (v1 ++ v2') (nsurfaces m1 + nsurfaces m2) (position m1) (verticeIndex m1 ++ verticeIndex2') (modelColor m1 ++ modelColor m2)
   

interTriangleLine :: V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> V3 GLfloat -> Bool
interTriangleLine a b c o d = abs det >= 1e-6 && t >= 0.0 && u >= 0.0 && v >= 0.0 && (u + v) <= 1.0
    where
        e1 = b - a
        e2 = c - a
        n = cross e1 e2
        det = - (dot d n)
        invdet = 1.0 / det
        ao = o - a
        dao = cross ao d
        u = dot e2 dao * invdet
        v = -dot e1 dao * invdet
        t = dot ao n * invdet

interModelLine :: Model -> V3 GLfloat -> V3 GLfloat -> Bool
interModelLine m o d = any inter (vertices m)
    where
        inter :: [GLfloat] -> Bool
        inter s = interTriangleLine (V3 (head s) (s !! 1) (s !! 2)) (V3 (s !! 3) (s !! 4) (s !! 5)) (V3 (s !! 6) (s !! 7) (s !! 8)) o d
