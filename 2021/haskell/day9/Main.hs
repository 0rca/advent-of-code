module Main where

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

low_point :: Int -> Int -> Vector (Vector Int) -> Maybe Int
low_point i j vec =
    let point = vec ! i ! j
        vicinity =
            concatMap
                maybeToList
                [ vec !?? (i, j - 1)
                , vec !?? (i, j + 1)
                , vec !?? (i - 1, j)
                , vec !?? (i + 1, j)
                ]
     in if point < minimum vicinity then Just point else Nothing

risk_level x = x + 1

low_points :: Vector (Vector Int) -> [Int]
low_points mtx = concatMap maybeToList [low_point i j mtx | i <- [0 .. Vector.length mtx - 1], j <- [0 .. Vector.length (mtx ! i) - 1]]

ans1 mtx = sum (risk_level <$> low_points mtx)
main = do
    lines <- read_heightmap
    -- mapM_ print lines
    let mtx = Vector.fromList (fmap Vector.fromList lines)

    putStrLn ("Answer 1: " <> show (ans1 mtx))
