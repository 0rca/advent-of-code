module Main where

import Control.Monad (forM_)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Stack (HasCallStack)

type Point = (Int, Int)
type Page = Set Point
data Fold = FoldY Int | FoldX Int deriving (Show)

readInput lines =
    let points = takeWhile (/= "") lines
        folds = tail (dropWhile (/= "") lines)
     in (collect (readPoint <$> points), readFold <$> folds)
  where
    readPoint :: [Char] -> Point
    readPoint str = read ("(" <> str <> ")")

    collect ps = Set.fromList ps

    readFold :: HasCallStack => [Char] -> Fold
    readFold str =
        let prefix = takeWhile (/= '=') str
            num = tail (dropWhile (/= '=') str)
         in case prefix of
                "fold along y" -> FoldY (read num)
                "fold along x" -> FoldX (read num)
                _ -> error ("invalid input: " <> show prefix)

printMap :: Page -> IO ()
printMap page =
    let maxX = fst $ maximumBy (compare `on` fst) page
        maxY = snd $ maximumBy (compare `on` snd) page
     in forM_ [0 .. maxY] $ \j -> do
            forM_ [0 .. maxX] $ \i ->
                if Set.member (i, j) page
                    then putStr "#"
                    else putStr "."
            putStrLn ""

fold :: Fold -> Page -> Page
fold (FoldY y) = Set.fromList . fmap (foldY y) . Set.toList
  where
    foldY y (i, j)
        | j > y = (i, 2 * y - j)
        | otherwise = (i, j)
fold (FoldX x) = Set.fromList . fmap (foldX x) . Set.toList
  where
    foldX x (i, j)
        | i > x = (2 * x - i, j)
        | otherwise = (i, j)

getInput = do
    ls <- lines <$> getContents
    return (readInput ls)

main = do
    (page, folds) <- getInput
    print (Set.size (fold (head folds) page))

-- printMap (fold (FoldX 5) . fold (FoldY 7) $ page)
