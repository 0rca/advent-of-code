module Main where
import Data.Foldable (foldl', maximum, minimum)

solutions :: (Int -> Int -> Int) -> [Int] -> [Int]
solutions computeFuel input =
  let minp = minimum input
      maxp = maximum input
      fuel x = foldl' (\acc y -> acc + computeFuel x y) 0 input
  in fmap fuel [minp .. maxp]

simpleFuel x y = abs (x - y)

complexFuel x y =
  let minp = min x y
      maxp = max x y
  in (1 + maxp - minp) * (maxp - minp) `div` 2

readInput :: IO [Int]
readInput = do
  line <- getContents
  return (read ("[" <> line <> "]"))

main = do
  input <- readInput
  putStrLn ("Answer 1: " <> show (minimum (solutions simpleFuel input)))
  putStrLn ("Answer 2: " <> show ((minimum (solutions complexFuel input))))

