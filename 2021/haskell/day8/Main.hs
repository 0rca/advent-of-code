{-# LANGUAGE FlexibleContexts #-}
module Main where

import Data.Foldable (foldl', maximum, minimum)

readInput :: IO [[String]]
readInput = do
  ls <- lines <$> getContents
  return (words . drop 2 . dropWhile (/= '|') <$> ls)

ans1 :: [[String]] -> Integer
ans1 input = sum $ fmap sum $ fmap count1478 <$> input

  where
    count1478 str = if length str `elem` [2, 4, 3, 7] then 1 else 0

main = do
  input <- readInput
  print (ans1 input)
