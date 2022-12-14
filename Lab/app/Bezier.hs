module Bezier where

import Model
import Linear
import Graphics.GL

bezier :: Float -> [Model] -> [Model]
bezier t mds = head mds : [enlargeModel (deltaB * i / t) (rotateModel (interModel i) (slerp qua1 qua2 (i / t)))
                            | i <- [0 .. t]]
    where
        p1 = position (mds !! 1)
        p2 = position (mds !! 2)
        p3 = position (mds !! 3)
        p4 = position (mds !! 4)
        interp :: Float -> V3 GLfloat
        interp t = (1 - t)^3 *^ p1 + (3 * (1 - t)^2 * t) *^ p2 + (3 * (1 - t) * t^2) *^ p3 + t^3 *^ p4
        n = length mds - 1

        qua1 = axisAngle (V3 1 0 0) 0
        qua2 = quaternion (mds !! 4) * (1 / quaternion (mds !! 1))

        deltaB = scale (mds !! 4) / scale (mds !! 1) - 1

        interModel :: Float -> Model 
        interModel i = Model
                            (vertices (mds !! 1))
                            (nsurfaces (mds !! 1))
                            (interp (i / t))
                            (modelColor (mds !! 1))
                            (quaternion (mds !! 1))
                            1
    

                         
