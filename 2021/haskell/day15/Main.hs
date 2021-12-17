{-# LANGUAGE BlockArguments #-}

module Main where

import Data.Foldable (Foldable (foldl'), forM_, maximumBy)
import Data.Function (on)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Text.Printf
import Data.List (foldl1')
import Control.Monad (when)
import Data.Maybe (maybeToList)

type Point = (Int, Int)
type Cave = Map Point Int

maxX :: Cave -> Int
maxX cave = fst $ maximumBy (compare `on` fst) (Map.keys cave)

maxY :: Cave -> Int
maxY cave = snd $ maximumBy (compare `on` snd) (Map.keys cave)

readInput :: IO Cave
readInput = do
    ls <- lines <$> getContents
    return $ Map.fromList do
        (i, row) <- zip [0 ..] ls
        (j, val) <- zip [0 ..] row
        pure ((i, j), read [val])

showCave cave = do
    forM_ [0 .. maxX cave] $ \i -> do
        forM_ [0 .. maxY cave] $ \j ->
            case Map.lookup (i, j) cave of
                Just v -> putStr (printf "%5d" v)
                Nothing -> putStr (printf "%5s" "")
        putStrLn ""

mapRisk cave acc = foldl' foo acc [(i, j) | i <- [0 .. maxX cave], j <- [0 .. maxY cave]]
  where
    foo acc (0, 0) = Map.insert (0, 0) 0 acc
    foo acc (i, j) = Map.insert (i, j) (cave ! (i, j) + minOf acc [(i, j - 1), (i, j + 1), (i - 1, j), (i + 1, j)]) acc


minOf :: Cave -> [Point] -> Int
minOf cave =  minimum . concatMap (\p -> maybeToList (Map.lookup p cave))

addRisk n = fmap (\r -> 1 + ((r - 1) + n) `mod` 9)

translate (x, y) = Map.mapKeys (\(i, j) -> (i + x, j + y))

largerMap :: Cave -> Cave
largerMap cave =
    let sizeX = maxX cave + 1
        sizeY = maxY cave + 1
        copy n m = addRisk (n + m) . translate (n * sizeX, m * sizeY)
        in foldl1' Map.union [copy n m cave | n <- [0..4], m <- [0..4]]

settle (x0:x1:xs)
    | x0 == x1 = x0
    | otherwise = settle (x1:xs)
settle _ = error "unreachable: infinite list"

main = do
    cave <- readInput
    -- when (addRisk 0 cave /= cave) (fail "risk calculation incorrect")
    -- when (addRisk 9 cave /= cave) (fail "risk calculation incorrect")
    -- showCave cave
    -- putStrLn ""
    let riskMap = settle (iterate (mapRisk cave) Map.empty)

    putStrLn (printf "Answer 1: %d" (riskMap ! (maxX cave, maxY cave)))

    let cave' = largerMap cave
        riskMap' = settle (iterate (mapRisk cave') Map.empty)
    putStrLn (printf "Answer 2: %d" (riskMap' ! (maxX cave', maxY cave')))

