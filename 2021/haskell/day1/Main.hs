module Main where

import Data.Function (fix)
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
            print =<< ans3 <$> readInts
        ["2b"] ->
            print =<< ans4 <$> readInts
        ["2c"] ->
            print =<< ans5 <$> readInts
        _ ->
            print ("Usage: cabal run day1 1|2|2a|2b|2c < input.txt")

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
        , ans3 input2 == 5
        , ans4 input2 == 5
        , ans5 input2 == 5
        ]

{-
  Brute force solution: generate list of windows, sum the numbers, feed into
  solution for part I
-}
ans3 xs = ans1 (map sum (windows 3 xs))

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
ans4 xs = ans1 (sums 3 xs)

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
ans5 xs =
    ans1
        ( fix
            ( \rec xs ->
                if length xs < 3
                    then []
                    else scanl1 (+) xs !! 2 : rec (tail xs)
            )
            xs
        )
