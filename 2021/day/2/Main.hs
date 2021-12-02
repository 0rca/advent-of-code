{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Foldable (foldl')
import System.Environment

type Instruction = (String, Int)

readInstructions :: IO [Instruction]
readInstructions = do
  ls <- lines <$> getContents
  return (map parseLine ls)
  where
    parseLine l = case words l of
      [dir, s] -> (dir, read s)
      _ -> error ("invalid input: " <> l)

main = do
  args <- getArgs
  case args of
    ["1"] ->
      print . ans1 =<< readInstructions
    ["2"] ->
      print . ans2 =<< readInstructions
    ["1a"] ->
      print . ans1' =<< readInstructions
    ["2a"] ->
      print . ans2' =<< readInstructions
    _ ->
      print "Usage: cabal run day2 1|1a|2|2a < input.txt"

-- | straightforward solution
ans1 instructions =
  let (x, y) = foldl' interpret (0, 0) instructions
   in x * y
  where
    interpret (x, y) ("forward", i) = (x + i, y)
    interpret (x, y) ("down", i) = (x, y + i)
    interpret (x, y) ("up", i)
      | y >= i = (x, y - i)
    interpret pos i = error ("invalid instruction at " <> show pos <> ": " <> show i)

input1 = [("forward", 5), ("down", 5), ("forward", 8), ("up", 3), ("down", 8), ("forward", 2)]

test = ans1 input1 == 150 && ans2 input1 == 900

-- | straightforward solution again
ans2 instructions =
  let (x, y, _) = foldl' interpret (0, 0, 0) instructions
   in x * y
  where
    interpret (x, y, a) ("forward", i) = (x + i, y + i * a, a)
    interpret (x, y, a) ("down", i) = (x, y, a + i)
    interpret (x, y, a) ("up", i) = (x, y, a - i)
    interpret pos i = error ("invalid instruction at " <> show pos <> ": " <> show i)

-------------------------------------------------------------------------------
-- WARNING: Over-engineered interpreters beyond this point
--

data ST1 = ST1 {x :: !Int, y :: !Int} deriving (Show)

data ST2 = ST2 {x :: !Int, y :: !Int, a :: !Int} deriving (Show)

class Interpreter st where
  interpret :: st -> Instruction -> st
  extract :: st -> Int

ans1' = genericAns ST1 {x = 0, y = 0}

ans2' = genericAns ST2 {x = 0, y = 0, a = 0}

instance Interpreter ST1 where
  interpret st@ST1 {x, y} ("forward", i) = st {x = x + i}
  interpret st@ST1 {x, y} ("down", i) = st {y = y + i}
  interpret st@ST1 {x, y} ("up", i) = st {y = y - i}
  interpret pos i = error ("invalid instruction at " <> show pos <> ": " <> show i)

  extract ST1 {x, y} = x * y

instance Interpreter ST2 where
  interpret st@ST2 {x, y, a} ("forward", i) = st {x = x + i, y = y + i * a}
  interpret st@ST2 {x, y, a} ("down", i) = st {a = a + i}
  interpret st@ST2 {x, y, a} ("up", i) = st {a = a - i}
  interpret pos i = error ("invalid instruction at " <> show pos <> ": " <> show i)

  extract ST2 {x, y} = x * y

genericAns :: Interpreter st => st -> [Instruction] -> Int
genericAns state instructions = extract (foldl' interpret state instructions)
