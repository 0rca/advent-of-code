{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Either (isLeft, isRight)
import Data.Foldable (find, foldl')
import Data.Maybe (maybeToList)
import Data.Monoid (Sum (getSum))
import System.Environment (getArgs)

data T a = R a | C a deriving (Show)
type Row = (Int, T [Either Int Int])
type State = [Row]
type Board = [[Int]]

type Solution = [Int] -> State -> Maybe Int

ans1 :: Solution
ans1 input state =
  case winnerScores input state of
    [] -> Nothing
    xs -> Just (head xs)

ans2 :: Solution
ans2 input state =
  case winnerScores input state of
    [] -> Nothing
    xs -> Just (last xs)

-- >>> winnerScores input boards
-- [4512,2192,1924]

winnerScores :: [Int] -> State -> [Int]
winnerScores [] state = []
winnerScores _ [] = []
winnerScores (i : is) state =
  let state' = draw i state
   in case winners state' of
        [] -> winnerScores is state'
        ws ->
          let state'' = filter (\(board, _) -> board `notElem` ws) state'
           in concatMap (\w -> maybeToList (finalScore i w state')) ws ++ winnerScores is state''
 where
  finalScore :: Int -> Int -> [Row] -> Maybe Int
  finalScore i w state =
    let rows = lookupWinner w state
     in case concatMap unmarkedNumbers rows of
          [] -> Nothing
          xs -> Just (i * sum xs)

  unmarkedNumbers :: Row -> [Int]
  unmarkedNumbers (_, R xs) = concatMap onlyLeft xs
  unmarkedNumbers _ = []

  onlyLeft :: Either b a -> [b]
  onlyLeft (Left i) = [i]
  onlyLeft _ = []

class Draw a where
  draw :: Int -> a -> a

instance Draw [Either Int Int] where
  draw n xs = fmap f xs
   where
    f (Left i)
      -- flip the number
      | i == n = Right i
    f x = x

instance Draw Row where
  draw n (i, C xs) = (i, C (draw n xs))
  draw n (i, R xs) = (i, R (draw n xs))

instance Draw State where
  draw n xs = fmap (draw n) xs

winners :: State -> [Int]
winners rs = fst <$> filter wins rs
 where
  wins (_, C xs) = all isRight xs
  wins (_, R xs) = all isRight xs

lookupWinner :: Int -> State -> [Row]
lookupWinner b rs = filter (rowsOf b) rs
 where
  rowsOf b (n, R xs) = b == n
  rowsOf _ _ = False

main = do
  args <- getArgs
  case args of
    ["1"] -> do
      (input, boards) <- readInput
      -- print input
      -- mapM_ print boards
      print (ans1 input boards)
    ["2"] -> do
      (input, boards) <- readInput
      -- print input
      -- mapM_ print boards
      print (ans2 input boards)
    _ ->
      print ("Usage: cabal run day4 1|2 < input.txt")

------------------------ Input Parsers -----------------------------------------
readInput :: IO ([Int], State)
readInput = do
  ls <- lines <$> getContents
  let input = read ("[" <> head ls <> "]")
      (_, _, boards) = finalize (foldl' collectBoards (1, [], []) ((tail (tail ls))))
  return (input, concat (reverse boards))
 where
  collectBoards (n, rows, boards) line =
    case line of
      "" -> finalize (n, rows, boards)
      _ -> (n, readInts line : rows, boards)

  finalize (n, rows, boards) =
    if rows /= []
      then (n + 1, [], compileBoard n (reverse rows) : boards)
      else (n, rows, boards)

-- >>> readInts "22 13 17 11  0"
-- [22,13,17,11,0]
readInts :: String -> [Int]
readInts input =
  case reads input of
    [(i, input')] -> i : readInts input'
    _ -> []

compileBoard :: Int -> Board -> State
compileBoard i xs =
  let rows = xs
      cols = transpose5 rows
      mkRel c = fmap (\r -> (i, c (fmap Left r)))
   in mkRel R rows <> mkRel C cols

transpose5 :: Show a => [[a]] -> [[a]]
transpose5 [a : as, b : bs, c : cs, d : ds, e : es] =
  [a, b, c, d, e] : transpose5 [as, bs, cs, ds, es]
transpose5 [[], _, _, _, _] = []
transpose5 [_, [], _, _, _] = []
transpose5 [_, _, [], _, _] = []
transpose5 [_, _, _, [], _] = []
transpose5 [_, _, _, _, []] = []
transpose5 e = error ("invalid input: " <> show e)

------------------------ Tests -------------------------------------------------
-- >>> test
-- True
test = ans1 input boards == Just 4512

board1, board2, board3 :: Board
board1 =
  [ [22, 13, 17, 11, 0]
  , [8, 2, 23, 4, 24]
  , [21, 9, 14, 16, 7]
  , [6, 10, 3, 18, 5]
  , [1, 12, 20, 15, 19]
  ]
board2 =
  [ [3, 15, 0, 2, 22]
  , [9, 18, 13, 17, 5]
  , [19, 8, 7, 25, 23]
  , [20, 11, 10, 24, 4]
  , [14, 21, 16, 12, 6]
  ]
board3 =
  [ [14, 21, 17, 24, 4]
  , [10, 16, 15, 9, 19]
  , [18, 8, 23, 26, 20]
  , [22, 11, 13, 6, 5]
  , [2, 0, 12, 3, 7]
  ]

boards :: State
boards = compileBoard 1 board1 <> compileBoard 2 board2 <> compileBoard 3 board3

input :: [Int]
input = [7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22, 18, 20, 8, 19, 3, 26, 1]
