{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (sort)
import System.Environment

readInput = do
    ls <- lines <$> getContents
    return (parseInput <$> ls)
  where
    parseInput input =
        let dict = words $ takeWhile (/= '|') input
            signal = tail (words $ dropWhile (/= '|') input)
         in (dict, signal)

----------- Part I : Simple iteration -----------------------------------------

ans1 :: [[String]] -> Integer
ans1 input = sum $ fmap sum $ fmap count1478 <$> input
  where
    count1478 str = if length str `elem` [2, 4, 3, 7] then 1 else 0

----------- Part II : Horrible deduction

diff xs ys = filter (`notElem` ys) xs -----------------------------------------

makeDict :: [[Char]] -> HashMap [Char] Int
makeDict codes = invert (parse codes IntMap.empty)
  where
    invert = Map.fromList . fmap (\(a, b) -> (b, a)) . IntMap.toList

parse :: [[Char]] -> IntMap [Char] -> IntMap [Char]
parse ([a, b] : xs) board = parse xs (IntMap.insert 1 (sort [a, b]) board)
parse ([a, b, c] : xs) board = parse xs (IntMap.insert 7 (sort [a, b, c]) board)
parse ([a, b, c, d] : xs) board = parse xs (IntMap.insert 4 (sort [a, b, c, d]) board)
parse ([a, b, c, d, e, f, g] : xs) board = parse xs (IntMap.insert 8 (sort [a, b, c, d, e, f, g]) board)
parse (x : xs) board
    | length x == 6 =
        case length . diff x <$> IntMap.lookup 1 board of
            Just 4 ->
                case length . diff x <$> IntMap.lookup 4 board of
                    Just 2 ->
                        parse xs (IntMap.insert 9 (sort x) board)
                    Just 3 ->
                        parse xs (IntMap.insert 0 (sort x) board)
                    Nothing ->
                        parse (xs ++ [x]) board
                    _ ->
                        error ("not implemented: " <> show x)
            Just 5 ->
                parse xs (IntMap.insert 6 (sort x) board)
            Nothing ->
                parse (xs ++ [x]) board
            _ -> error ("#1 not implemented: " <> show x)
    | length x == 5 =
        case length . diff x <$> IntMap.lookup 7 board of
            Just 2 ->
                parse xs (IntMap.insert 3 (sort x) board)
            Just 3 ->
                case length . diff x <$> IntMap.lookup 4 board of
                    Just 3 -> parse xs (IntMap.insert 2 (sort x) board)
                    Just 2 -> parse xs (IntMap.insert 5 (sort x) board)
                    Nothing -> parse (xs ++ [x]) board
                    _ -> error ("#2.1 not implemented: " <> show x)
            Nothing ->
                parse (xs ++ [x]) board
            _ -> error ("#2.2 not implemented: " <> show x)
    | otherwise = error ("#3 not implemented: " <> show x)
parse [] board = board

interpret signals dict =
    listToInt (foldl (\acc x -> dict Map.! x : acc) [] (sort <$> signals))

listToInt (x : xs) = x + 10 * listToInt xs
listToInt [] = 0

main = do
    args <- getArgs
    case args of
        ["1"] -> do
            lines <- readInput
            let signals = fmap (\(_, x) -> x) lines
            putStrLn ("Answer 1: " <> show (ans1 signals))
        ["2"] -> do
            lines <- readInput
            let ans2 = sum $ fmap (\(codes, signals) -> interpret signals (makeDict codes)) lines
            putStrLn ("Answer 2: " <> show ans2)
        _ ->
            print ("Usage: cabal run day9 1|2 < input.txt")
