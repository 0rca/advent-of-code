module Main where

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
        _ ->
            print ("Usage: cabal run day1 1|2 < input.txt")

input1 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

ans1 :: [Int] -> Int
ans1 xs = go xs 0
  where
    go (x0 : x1 : xs) r
        | x1 > x0 = go (x1 : xs) (r + 1)
        | otherwise = go (x1 : xs) r
    go _ r = r

input2 = [607, 618, 618, 617, 647, 716, 769, 792]

ans2 :: [Int] -> Int
ans2 xs = go xs 0
  where
    go (x0 : x1 : x2 : x3 : xs) r
        | x3 > x0 = go (x1 : x2 : x3 : xs) (r + 1)
        | otherwise = go (x1 : x2 : x3 : xs) r
    go _ r = r

test = and [ans1 input1 == 7, ans2 input2 == 5, ans3 input2 == 5]

--

ans3 xs = ans1 (map sum (windows 3 xs))

windows n xs =
    case takeExactly n xs of
        Just xs' -> xs' : windows n (tail xs)
        Nothing -> []

takeExactly :: Int -> [a] -> Maybe [a]
takeExactly n xs = go n [] xs
  where
    go 0 ys _        = Just (reverse ys)
    go _ _  []       = Nothing
    go n ys (x : xs) = go (n - 1) (x : ys) xs

