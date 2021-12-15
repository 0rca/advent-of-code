{-# LANGUAGE TupleSections #-}

module Main where

import Data.Foldable (foldl', forM_)
import qualified Data.Foldable as Map
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- | reverse function application: same as ($) but the other way around
x & f = f x

type Recipe = Map (Char, Char) Char

data Polymer = Polymer
    { pairs :: Map (Char, Char) Int
    , singles :: Map Char Int
    }
    deriving (Show)

makePolymer :: [Char] -> Polymer
makePolymer chars =
    let pairs = Map.fromListWith (+) $ zipWith (\c0 c1 -> ((c0, c1), 1)) chars (tail chars)
        singles = Map.fromListWith (+) (fmap (,1) chars)
     in Polymer pairs singles

readEverything :: IO (Polymer, Recipe)
readEverything = do
    ls <- lines <$> getContents
    let polymer = makePolymer (head ls)
        insertions = parseInsertions (drop 2 ls)
    return (polymer, insertions)
  where
    parseInsertions lines = Map.fromList $ do
        l <- lines
        let [x0, x1] = take 2 l
            right = List.last l
        return ((x0, x1), right)

insert :: Recipe -> Polymer -> Polymer
insert dict poly = Map.foldlWithKey' mutate poly (pairs poly)
  where
    mutate :: Polymer -> (Char, Char) -> Int -> Polymer
    mutate acc pair@(l, r) pairCount
        | pairCount > 0 =
            let newChar = dict ! pair
                newPairs =
                    pairs acc & Map.adjust (\n -> n - pairCount) pair
                        & Map.insertWith (+) (l, newChar) pairCount
                        & Map.insertWith (+) (newChar, r) pairCount
                newSingles = singles acc & Map.insertWith (+) newChar pairCount
             in Polymer{pairs = newPairs, singles = newSingles}
        | otherwise = acc

polyLen poly = foldl' (+) 0 (singles poly)

polymers :: Recipe -> Polymer -> [Polymer]
polymers dict = iterate (insert dict)

signature :: Polymer -> Int
signature pol = maximum (singles pol) - minimum (singles pol)

main = do
    (template, dict) <- readEverything
    let seq = polymers dict template
    -- forM_ [0 .. 10 ] $ \i -> do
    --     print (i, polyLen (seq !! i))

    putStrLn ("Answer 1: " <> show (signature (seq !! 10)))
    putStrLn ("Answer 2: " <> show (signature (seq !! 40)))
