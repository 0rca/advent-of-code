module Main where

import Control.Monad (forM_)

import Data.Foldable (foldl')
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import System.Console.ANSI

type Cave a = Map (Int, Int) a

readInput = do
    ls <- lines <$> getContents
    let rows = fmap (\c -> fromEnum c - fromEnum '0') <$> ls
    return $ Map.fromList [((i, j), rows !! i !! j) | i <- [0 .. 9], j <- [0 .. 9]]

step cave = settle (iterate (extend tick) (next cave))
  where
    settle (x0 : x1 : xs)
        | x0 == x1 = x0
        | otherwise = settle (x1 : xs)
    settle _ = error "unreachable: infinite list"

next :: Cave Int -> Cave Int
next cave = fmap (+ 1) cave

tick :: (Int, Int) -> Cave Int -> Cave Int
tick (i, j) cave =
    let p = cave ! (i, j)
     in if p > 9
            then flash (i, j) cave
            else cave

flash :: (Int, Int) -> Cave Int -> Cave Int
flash (i, j) cave =
    let neighbourhood =
            [ (i - 1, j - 1)
            , (i - 1, j)
            , (i - 1, j + 1)
            , (i, j - 1)
            , (i, j + 1)
            , (i + 1, j - 1)
            , (i + 1, j)
            , (i + 1, j + 1)
            ]
     in foldl' (\acc p -> Map.adjust maybeInc p acc) (Map.insert (i, j) 0 cave) neighbourhood
  where
    maybeInc 0 = 0
    maybeInc x = x + 1

extend :: ((Int, Int) -> Cave a -> Cave a) -> Cave a -> Cave a
extend f cave = foldl' (\cave p -> f p cave) cave (Map.keys cave)

countFlashes :: Cave Int -> Int
countFlashes cave =
    sum (fmap (\x -> if x == 0 then 1 else 0) cave)

printState :: [Cave Int] -> Int -> IO ()
printState states n = do
    putStrLn ("--- " <> show n <> " ---")
    showCave (states !! n)

showCave :: Cave Int -> IO ()
showCave cave = do
    forM_ [0 .. 9] $ \i -> do
        forM_ [0 .. 9] $ \j -> do
            let x = cave ! (i, j)
            case x of
                0 ->
                    setSGR [Reset]
                _ ->
                    setSGR [SetConsoleIntensity FaintIntensity]
            putStr (show x)
        putStrLn ""

main = do
    cave <- readInput
    let states = iterate step cave

    mapM_ (printState states) [0 .. 9]
    mapM_ (printState states) [10, 20 .. 100]

    putStrLn ("Answer 1: " <> show (sum (countFlashes <$> take 101 states)))
