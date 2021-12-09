module Main where

import Data.List (nub, sort, sortBy)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as Vector

read_heightmap :: IO [[Int]]
read_heightmap = do
    ls <- lines <$> getContents
    return (fmap (\c -> fromEnum c - fromEnum '0') <$> ls)

(!??) vec (i, j) =
    case vec !? i of
        Nothing -> Nothing
        Just vec -> vec !? j

low_point :: Int -> Int -> Vector (Vector Int) -> Maybe (Int, Int, Int)
low_point i j vec =
    let point = vec ! i ! j
        vicinity = fmap (\(_, _, i) -> i) (vicinity_of i j vec)
     in if point < minimum vicinity then Just (i, j, point) else Nothing

risk_level (_, _, x) = x + 1

low_points :: Vector (Vector Int) -> [(Int, Int, Int)]
low_points mtx = concatMap maybeToList [low_point i j mtx | i <- [0 .. Vector.length mtx - 1], j <- [0 .. Vector.length (mtx ! i) - 1]]

vicinity_of i j vec =
    concatMap
        maybeToList
        [ fmap (\x -> (i, j - 1, x)) (vec !?? (i, j - 1))
        , fmap (\x -> (i, j + 1, x)) (vec !?? (i, j + 1))
        , fmap (\x -> (i - 1, j, x)) (vec !?? (i - 1, j))
        , fmap (\x -> (i + 1, j, x)) (vec !?? (i + 1, j))
        ]

higher_points i j vec =
    let point = vec ! i ! j
        vicinity = vicinity_of i j vec
     in nub $ (make_basin_for point) `concatMap` vicinity
  where
    make_basin_for origin (i, j, height) =
        if height >= 9
            then []
            else
                if height <= origin
                    then []
                    else (i, j, height) : higher_points i j vec

getHeight (_, _, h) = h

main = do
    lines <- read_heightmap
    let mtx = Vector.fromList (fmap Vector.fromList lines)
    -- mapM_ print mtx

    let lowest_points = low_points mtx
    putStrLn ("Answer 1: " <> show (sum (risk_level <$> lowest_points)))

    let basins = fmap (\(i, j, h) -> (i, j, h) : higher_points i j mtx) lowest_points
    putStrLn ("Answer 2: " <> show (product (take 3 (sortBy (\a b -> compare b a) (fmap length basins)))))
