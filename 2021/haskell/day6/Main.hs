module Main where

import Control.Monad (forM_)
import Data.Array.IO
import Data.Array.MArray
import System.Environment

{- | this solution uses unboxed mutable arrays: who said Haskell cannot do this?
 shift all counters towards 0, then modify 6 and 8 cell with new numbers
-}
step :: IOUArray Int Int -> IO ()
step arr = do
    new_spawns <- readArray arr 0
    forM_ [0 .. 7] $ \i -> writeArray arr i =<< readArray arr (i + 1)
    updateArray arr 6 (+ new_spawns)
    writeArray arr 8 new_spawns
    pure ()

-- | new unboxed mutable array of size 9, initialized from input list
initState :: [Int] -> IO (IOUArray Int Int)
initState xs = do
    arr <- newArray (0, 8) 0
    forM_ xs $ \i -> updateArray arr i (+ 1)
    return arr

-- | conveniently modifies one array cell with a function
updateArray :: IOUArray Int Int -> Int -> (Int -> Int) -> IO ()
updateArray arr i f = writeArray arr i =<< f <$> readArray arr i

-- | does the action i times
repeatedly :: Int -> IO () -> IO ()
repeatedly i action = forM_ [1 .. i] $ \_ -> action

-- | the fastest hack to parse a line of comma-separated ints
readInts :: IO [Int]
readInts = do
    line <- getContents
    return (read ("[" <> line <> "]"))

main = do
    arr <- initState =<< readInts

    -- simulate what happens after 80 steps
    repeatedly 80 (step arr)
    getElems arr >>= \xs ->
        putStrLn ("1. After 80 days: " <> show (sum xs))

    -- simulate what happens after the 256 total steps, using the same array
    repeatedly (256 - 80) (step arr)
    getElems arr >>= \xs ->
        putStrLn ("2. After 256 days: " <> show (sum xs))
