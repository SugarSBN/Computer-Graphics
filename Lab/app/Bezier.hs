module Bezier where

import Model

beizier :: [Model] -> [Model]
beizier mds = if n >= 600 then mds else beizier mds'
    where
        n = length mds - 1
        mds' = (take 2 mds) ++ [
                                Model
                                    (vertices (mds !! i))
                                    (nsurfaces (mds !! i))
                                    ((position (mds !! i)) + (position (mds !! (i - 1))) / 2)
                                    (modelColor (mds !! i))
                                    (theta (mds !! i))
                                    (phi (mds !! i))
                                | i <- [2 .. n]
                               ] ++ [mds !! n]
