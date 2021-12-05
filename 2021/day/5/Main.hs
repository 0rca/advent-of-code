module Main where

{--
--- Day 5: Hydrothermal Venture ---
You come across a field of hydrothermal vents on the ocean floor! These vents
constantly produce large, opaque clouds, so it would be best to avoid them if
possible.

They tend to form in lines; the submarine helpfully produces a list of nearby
lines of vents (your puzzle input) for you to review. For example:

```
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
```

Each line of vents is given as a line segment in the format `x1,y1 -> x2,y2`
where `x1,y1` are the coordinates of one end the line segment and `x2,y2` are
the coordinates of the other end. These line segments include the points at both ends.
In other words:

An entry like `1,1 -> 1,3` covers points `1,1, 1,2`, and `1,3`.
An entry like `9,7 -> 7,7` covers points `9,7, 8,7`, and `7,7`.

For now, only consider horizontal and vertical lines: lines where either `x1 = x2` or `y1 = y2`.

So, the horizontal and vertical lines from the above list would produce the following diagram:

```
.......1..
..1....1..
..1....1..
.......1..
.112111211
..........
..........
..........
..........
222111....
```

In this diagram, the top left corner is `0,0` and the bottom right corner is `9,9`.
Each position is shown as the number of lines which cover that point or `.` if no line
covers that point. The top-left pair of 1s, for example, comes from `2,2 -> 2,1`;
the very bottom row is formed by the overlapping lines `0,9 -> 5,9` and `0,9 -> 2,9`.

To avoid the most dangerous areas, you need to determine the number of points
where at least two lines overlap. In the above example, this is anywhere in the
diagram with a 2 or larger - a total of 5 points.

Consider only horizontal and vertical lines.
At how many points do at least two lines overlap?

--------------------------------------------------------------------------------

First, let us decide on what data structure would be useful here. Because we
don't know about dimensions of the map without looking at the input, let's
try using a `Map (Int, Int) Int` that will keep number of overlaps at specific
coordinate.
--}

import Data.Char (isDigit)
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Environment

type Point = (Int, Int)
type Field = Map Point Int

{-
To add a point into the map, we insert `1` at specific coordinate. If there is
already a number, we add to it.
-}

addPoint :: Field -> Point -> Field
addPoint map pt = Map.insertWith (+) pt 1 map

-- Let's also have a type for representing lines: a list of points should do fine.

type Line = [Point]

{-
We can add several points at once using `foldl'`: `foldl' (b -> a -> b) -> b -> [a] -> b`
-}
addLine :: Field -> Line -> Field
addLine map points = foldl' addPoint map points

{-
Now that we have the map and we know how to construct it, we can find the
answer for the first part.

  At how many points do at least two lines overlap?

This can also be implemented using a `foldl'` for `Map`: `foldl' :: (a -> b -> a) -> a -> Map k b -> a`
-}

ans1 :: Field -> Int
ans1 map = Map.foldl' (count (>= 2)) 0 map
 where
  count pred acc nOverlaps =
    if pred nOverlaps then acc + 1 else acc

-- Let's test this function against a dummy field:

test = ans1 (addLine Map.empty [(0, 0), (0, 0), (1, 1), (1, 1), (10, 10)])

-- >>> test
-- 2

-- Let's use this hack that turns input string into readable pair of tuples (points)
-- >>> reformat "0,9 -> 5,9"
-- "((0,9 ), ( 5,9))"

reformat :: String -> String
reformat str = "((" <> concatMap replace str <> "))"
 where
  replace '-' = "),"
  replace '>' = " ("
  replace c = [c]

readPoints :: String -> (Point, Point)
readPoints str = read (reformat str)

-- >>> readInts "0,9 -> 5,9"
-- [0,9,5,9]

-- Then turn the digits into a line:

mkLine ((x0, y0), (x1, y1))
  | (x1, y1) >= (x0, y0) = [(x, y) | x <- [x0 .. x1], y <- [y0 .. y1]]
  | otherwise = mkLine ((x1, y1), (x0, y0))

-- >>> mkLine [0, 0, 1, 1]

hvOnly ((x0, y0), (x1, y1)) = y0 == y1 || x0 == x1

-- >>> mkLine <$> (filter hvOnly (readInts <$> ["0,9 -> 5,9"]))

mkMap :: ((Point, Point) -> [Point]) -> [String] -> Field
mkMap mkPoints lines =
  let pts = map mkPoints (readPoints <$> lines)
   in foldl' addLine Map.empty pts

mkPoints1 :: (Point, Point) -> [Point]
mkPoints1 ps =
  if hvOnly ps
    then mkLine ps
    else []

mkPoints2 :: (Point, Point) -> [Point]
mkPoints2 ps =
  case mkPoints1 ps of
    [] ->
      mkDiagonal ps
    points ->
      points

main = do
  args <- getArgs
  case args of
    ["1"] -> do
      ls <- lines <$> getContents
      print (ans1 (mkMap mkPoints1 ls))
    ["2"] -> do
      ls <- lines <$> getContents
      print (ans1 (mkMap mkPoints2 ls))
    _ ->
      print ("Usage: cabal run day5 1|2 < input.txt")

------------------------------- Part II ----------------------------------------

-- Diagonal lines: if the ratio of x / y is 1

mkDiagonal ((x0, y0), (x1, y1)) =
  let dx = x1 - x0
      dy = y1 - y0
   in if dx == 0 || dy == 0 || abs dx /= abs dy
        then []
        else reverse (go (x0, y0) (x1, y1) (dx `div` abs dx, dy `div` abs dy) [])
 where
  go p0@(x0, y0) p1 (dx, dy) path
    | p0 == p1 = p1 : path
    | otherwise = go (x0 + dx, y0 + dy) p1 (dx, dy) (p0 : path)

-- >>> mkDiagonal (0,4) (4,0)
-- [(0,4),(1,3),(2,2),(3,1),(4,0)]
