module Model where

import ReadParse
import Graphics.GL
import Data.IORef
import Linear


data Model = Model {
    vertices    :: [[GLfloat]],
    nsurfaces   :: GLint,
    position    :: V3 GLfloat,
    modelColor  :: V4 GLfloat
}

readModel :: FilePath -> IO Model
readModel filename = do
    verts <- readVertices filename
    let mx = maximum (map abs (concat verts))
    return $ Model 
                (map (map (/ mx)) verts) 
                (fromIntegral (length verts)) 
                (V3 0.0 0.0 0.0)
                (V4 1.0 1.0 1.0 1.0)

translateModel :: Model -> V3 GLfloat -> Model
translateModel m v =
    Model 
        (vertices m) 
        (nsurfaces m)
        (position m + v)
        (modelColor m)

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
        pos = position m
        inter :: [GLfloat] -> Bool
        inter s = interTriangleLine 
                    (pos + V3 (head s) (s !! 1) (s !! 2)) 
                    (pos + V3 (s !! 3) (s !! 4) (s !! 5)) 
                    (pos + V3 (s !! 6) (s !! 7) (s !! 8)) 
                    o d

modifyColor :: Model -> (V4 GLfloat -> V4 GLfloat) -> Model
modifyColor m f = Model
                    (vertices m)
                    (nsurfaces m)
                    (position m)
                    (f (modelColor m))

modifyList :: [a] -> Int -> (a -> a) -> [a]
modifyList l n f = 
    case n of
      0 -> f (head l) : tail l
      _ -> head l : modifyList (tail l) (n - 1) f


-- | ------------------------------------------------------------------
packVertices :: [Model] -> [[GLint]]
packVertices mds = packVertices' mds 0 
    where
        packVertices' :: [Model] -> GLint -> [[GLint]]
        packVertices' [] n = []
        packVertices' mds' n = [(i + n) * 3 | i <- [0 .. (nsurfaces (head mds'))]] : packVertices' (tail mds') (n + nsurfaces (head mds'))

packColors :: [Model] -> [V4 GLfloat]
packColors [] = []
packColors mds = modelColor (head mds) : packColors (tail mds)

packModels :: [Model] -> Model
packModels = foldl1 combineModels
    where
        combineModels :: Model -> Model -> Model
        combineModels m1 m2 = Model
                                (v1 ++ v2')
                                (nsurfaces m1 + nsurfaces m2)
                                (position m1)
                                (modelColor m1)
            where
                v1 = vertices m1
                v2 = vertices m2
                (V3 p1x p1y p1z) = position m1
                (V3 p2x p2y p2z) = position m2
                deltaX = p2x - p1x
                deltaY = p2y - p1y
                deltaZ = p2z - p1z
                v2' = map (\s -> [head s + deltaX, (s !! 1) + deltaY, (s !! 2) + deltaZ,
                                  (s !! 3) + deltaX, (s !! 4) + deltaY, (s !! 5) + deltaZ,
                                  (s !! 6) + deltaX, (s !! 7) + deltaY, (s !! 8) + deltaZ]) v2
                

