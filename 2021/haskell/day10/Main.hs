module Main where

import Data.Foldable (find)
import Data.List (sort)
import Data.Maybe (maybeToList)

read_lines :: IO [String]
read_lines = lines <$> getContents

openers = "([{<"
terminators = ")]}>"

score :: Char -> Int
score c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> error ("invalid character: " <> show c)

terminates :: Char -> Char -> Bool
terminates ')' '(' = True
terminates ']' '[' = True
terminates '}' '{' = True
terminates '>' '<' = True
terminates _ _ = False

terminator :: Char -> Char
terminator c =
  case find (`terminates` c) terminators of
    Just x -> x
    Nothing -> error ("invalid input :" <> show c)

data Result = Corrupt Char | Incomplete [Char] | Ok deriving (Show)

parse :: [Char] -> Result
parse [] = Ok
parse xs = go [] xs
 where
  go [] [] = Ok
  go acc [] = Incomplete acc
  go acc@[] (x : xs)
    | x `elem` terminators = Corrupt x
    | otherwise = go (x : acc) xs
  go acc@(s : ss) (x : xs)
    | x `terminates` s = go ss xs
    | x `elem` openers = go (x : acc) xs
    | otherwise = Corrupt x

ans1 lines =
  sum $ (maybeToList . maybeScore) `concatMap` (parse <$> lines)
 where
  maybeScore c = case c of
    Corrupt i -> Just (score i)
    _ -> Nothing

autocomplete :: [Char] -> [Char]
autocomplete xs = fmap terminator xs

autocompleteScore :: [Char] -> Int
autocompleteScore xs = foldl accumulateScore 0 xs
 where
  accumulateScore acc x =
    5 * acc + case x of
      ')' -> 1
      ']' -> 2
      '}' -> 3
      '>' -> 4
      _ -> error ("invalid input: " <> show x)

ans2 input =
  let scores = sort $ (autocompleteScore . autocomplete) <$> extractIncomplete `concatMap` (parse <$> input)
      len = length scores
   in scores !! (len `div` 2)
 where
  extractIncomplete (Incomplete str) = [str]
  extractIncomplete _ = []

main = do
  ls <- read_lines
  putStrLn ("Answer 1: " <> show (ans1 ls))
  putStrLn ("Answer 2: " <> show (ans2 ls))
