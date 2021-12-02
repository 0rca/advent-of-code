module Day1 where

import Data.Monoid

main1 :: [Integer] -> Integer
main1 = getSum . foldMap (Sum . fuel)

main2 :: [Integer] -> Integer
main2 = getSum . foldMap (Sum . totalFuel)

fuel :: Integer -> Integer
fuel mass = mass `div` 3 - 2

-- mass = f m + f (f m) + f (f (f m))) + ...
totalFuel :: Integer -> Integer
totalFuel mass = sum . takeWhile (> 0) $ iterate fuel (fuel mass)


