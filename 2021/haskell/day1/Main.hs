{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Function (fix)
import Data.Functor.Foldable
import System.Environment

readInts :: IO [Int]
readInts = fmap read . lines <$> getContents

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["1"] ->
            print =<< ans1 <$> readInts
        ["2"] ->
            print =<< ans2 <$> readInts
        ["2a"] ->
            print =<< ans2a <$> readInts
        ["2b"] ->
            print =<< ans2b <$> readInts
        ["2c"] ->
            print =<< ans2c <$> readInts
        ["2d"] ->
            print =<< ans2d <$> readInts
        _ ->
            print ("Usage: cabal run day1 1|2|2a|2b|2c|2d < input.txt")

input1 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

{-
  Direct recursive solutions: pattern match on the list and keep the accumulator
-}
ans1 :: [Int] -> Int
ans1 xs = go xs 0
  where
    go (x0 : x1 : xs) r
        | x1 > x0 = go (x1 : xs) (r + 1)
        | otherwise = go (x1 : xs) r
    go _ r = r

ans1a :: [Int] -> Int
ans1a xs = flip cata (zip xs (tail xs)) $ \case
    Nil -> 0
    Cons (x0, x1) sum -> if x1 > x0 then succ sum else sum

input2 = [607, 618, 618, 617, 647, 716, 769, 792]

{-
  Direct recursive solution for windows of size 3
-}

ans2 :: [Int] -> Int
ans2 xs = go xs 0
  where
    go (x0 : x1 : x2 : x3 : xs) r
        | x3 > x0 = go (x1 : x2 : x3 : xs) (r + 1)
        | otherwise = go (x1 : x2 : x3 : xs) r
    go _ r = r

test =
    and
        [ ans1 input1 == 7
        , ans2 input2 == 5
        , ans2a input2 == 5
        , ans2b input2 == 5
        , ans2c input2 == 5
        ]

{-
  Brute force solution: generate list of windows, sum the numbers, feed into
  solution for part I
-}
ans2a xs = ans1 (map sum (windows 3 xs))

-- | Generates a list of windows of size n from the input list
windows n xs =
    case takeExactly n xs of
        Just xs' -> xs' : windows n (tail xs)
        Nothing -> []

-- | takes n elements of the list. Fails when there is not enough elements
takeExactly :: Int -> [a] -> Maybe [a]
takeExactly n xs = go n [] xs
  where
    go 0 ys _ = Just (reverse ys)
    go _ _ [] = Nothing
    go n ys (x : xs) = go (n - 1) (x : ys) xs

-- Same solution as before, but accumulates sums directly
ans2b xs = ans1 (sums 3 xs)

sums n xs =
    case sumExactly n xs of
        Just s -> s : sums n (tail xs)
        Nothing -> []

-- | sums n first elements of the input list
sumExactly :: Int -> [Int] -> Maybe Int
sumExactly n xs = go n 0 xs
  where
    go 0 acc _ = Just acc
    go _ _ [] = Nothing
    go n acc (x : xs) = go (n - 1) (x + acc) xs

{- | esoteric solution: scanl1 for sums, fixpoint function for factoring out
 recursion
-}
ans2c xs =
    ans1
        ( fix
            ( \rec xs ->
                if length xs < 3
                    then []
                    else scanl1 (+) xs !! 2 : rec (tail xs)
            )
            xs
        )

ans2d xs = fold go (zip xs (drop 3 xs))
  where
    go Nil = 0
    go (Cons (x0, x3) sum) = if x3 > x0 then sum + 1 else sum
