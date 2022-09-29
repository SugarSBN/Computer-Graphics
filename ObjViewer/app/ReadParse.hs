module ReadParse where

import System.IO
import System.Exit
import Control.Monad
import Data.List.Split
import Data.Tuple.Select

readFileLines :: FilePath -> IO [String]
readFileLines filename = openFile filename ReadMode >>= hGetLines
    where
        hGetLines handle = do
            eof <- hIsEOF handle
            if not eof
                then do
                    now <- hGetLine handle
                    lft <- hGetLines handle
                    return (now : lft)
                else do
                    return [""]

type Point = (Float, Float, Float)
type Surface = (Int, Int, Int)

getPoints :: [String] -> [Point]
getPoints input = [resize (read (filted !! i !! 1), read (filted !! i !! 2), read (filted !! i !! 3)) | i <- [0 .. (n - 1)]]
    where
        filted = filter (\s -> not (null s) && head s == "v") (map words input)
        n = length filted
        resize :: Point -> Point
        resize (a, b, c) = (a / 500, b / 500, c / 500)

getSurfaces :: [String] -> [Surface]
getSurfaces input = [(getIndex (filted !! i !! 1), getIndex (filted !! i !! 2), getIndex (filted !! i !! 3)) | i <- [0 .. (n - 1)]] 
    where
        filted = filter (\s -> not (null s) && head s == "f") (map words input)
        n = length filted
        getIndex :: String -> Int
        getIndex s = read $ head (splitOn "/" s)

genVertices :: [Surface] -> [Point] -> [Float]
genVertices surfaces points = concat $ map (getSur points) surfaces
    where
        getSur :: [Point] -> Surface -> [Float]
        getSur ps s = [sel1 (ps !! (sel1 s - 1)), sel2 (ps !! (sel1 s - 1)), sel3 (ps !! (sel1 s - 1))] ++
                      [sel1 (ps !! (sel2 s - 1)), sel2 (ps !! (sel2 s - 1)), sel3 (ps !! (sel2 s - 1))] ++
                      [sel1 (ps !! (sel3 s - 1)), sel2 (ps !! (sel3 s - 1)), sel3 (ps !! (sel3 s - 1))]


readVertices :: FilePath -> IO [Float]
readVertices filename = do
    lines <- readFileLines filename
    let points = getPoints lines
    let surfaces = getSurfaces lines
    return $ genVertices surfaces points
