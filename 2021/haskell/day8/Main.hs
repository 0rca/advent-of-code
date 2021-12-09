module Main where

import Data.HashMap.Strict (HashMap, (!))
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

----------- Part II : Horrible deduction using obscure properties of indicators
-- skip to next solutions for less eyebleed

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

----------- Part IIa : Can it be prettier? ------------------------------------

--  these outputs are mapped to indicator segments
--  +- a -+      0 1 2 3 4 5 6 7 8 9
--  |     |      a _ a a _ a a a a a  'a' occurs 8 times
--  b     c      b _ _ _ b b b _ b b  'b' occurs 6 times
--  |     |      c c c c c _ _ c c c  'c' occurs 8 times
--  +- d -+      _ _ d d d d d _ d d  'd' occurs 7 times
--  |     |      e _ e _ _ _ e _ e _  'e' occurs 4 times
--  e     f      f f _ f f f f f f f  'f' occurs 9 times
--  |     |      g _ g g _ g g _ g g  'g' occurs 7 times
--  +- g -+      : : : : : : : : : :
--               6 2 5 5 4 5 6 3 7 6 segments

-- | digits, numbers of segments, and segment signature: sorted list of segment occurences
decode :: [Int] -> Int
decode sig =
    case sig of
        -- 0 : 6 segments, [4,6,7,8,8,9]
        [4, 6, 7, 8, 8, 9] -> 0
        -- 1 : 2 segments, [8,9]
        [8, 9] -> 1
        -- 2 : 5 segments, [4,7,7,8,8]
        [4, 7, 7, 8, 8] -> 2
        -- 3 : 5 segments, [7,7,8,8,9]
        [7, 7, 8, 8, 9] -> 3
        -- 4 : 4 segments, [6,7,8,9]
        [6, 7, 8, 9] -> 4
        -- 5 : 5 segments, [6,7,7,8,9]
        [6, 7, 7, 8, 9] -> 5
        -- 6 : 6 segments, [4,6,7,7,8,9]
        [4, 6, 7, 7, 8, 9] -> 6
        -- 7 : 3 segments, [8,8,9]
        [8, 8, 9] -> 7
        -- 8 : 7 segments, [4,6,7,7,8,8,9]
        [4, 6, 7, 7, 8, 8, 9] -> 8
        -- 9 : 6 segments, [6,7,7,8,8,9]
        [6, 7, 7, 8, 8, 9] -> 9
        -- unknown segment signature
        _ -> error ("inconsistent signature :" <> show sig)

type Input = Char

type Board a = HashMap Input a

compileBoard :: [String] -> Board Int
compileBoard words =
    foldl (\acc char -> Map.insertWith (+) char 1 acc) Map.empty (concat words)

signature :: Board Int -> String -> [Int]
signature board str = sort (fmap (\char -> board ! char) str)

decrypt :: [String] -> [String] -> Int
decrypt patterns =
    listToInt . reverse . fmap (decode . signature (compileBoard patterns))

main = do
    args <- getArgs
    case args of
        ["1"] -> do
            lines <- readInput
            let signals = fmap snd lines
            putStrLn ("Answer 1: " <> show (ans1 signals))
        ["2"] -> do
            lines <- readInput
            let ans2 = sum $ fmap (\(codes, signals) -> interpret signals (makeDict codes)) lines
            putStrLn ("Answer 2: " <> show ans2)
        ["2a"] -> do
            inputs <- readInput
            let ans2a = sum $ fmap (\(patterns, signals) -> decrypt patterns signals) inputs
            putStrLn ("Answer 2a: " <> show ans2a)
        _ ->
            print "Usage: cabal run day9 1|2 < input.txt"
