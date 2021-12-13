module Main where

import Control.Monad (guard)
import qualified Data.Char as Char
import Data.List (findIndex, intersperse)
import qualified Debug.Trace as Debug

import Data.Map (Map)
import qualified Data.Map.Strict as Map

data Vertex = Start | End | Cave String deriving (Eq, Ord, Show)
type Edge = (Vertex, Vertex)
newtype Graph = Graph {unGraph :: [Edge]} deriving (Show)

isBigCave :: Vertex -> Bool
isBigCave (Cave c) = all Char.isUpper c
isBigCave _ = False

paths :: Vertex -> Vertex -> Graph -> ([Vertex] -> Bool) -> [Vertex] -> [[Vertex]]
paths a b (Graph edges) pred visited = do
    (c, d) <- edges
    guard (a == c)
    guard (isBigCave c || c `notElem` visited || pred visited)
    if b == d
        then return (reverse (d : c : visited))
        else paths d b (Graph edges) pred (c : visited)

visitPermitted :: [Vertex] -> Bool
visitPermitted xs = go Map.empty xs
  where
    go acc [] = True
    go acc (Start : xs) = go acc xs
    go acc (End : xs) = go acc xs
    go acc (x : xs)
        | isBigCave x = go acc xs
    go acc (x : xs) =
        case Map.lookup x acc of
            Nothing -> go (Map.insert x () acc) xs
            Just () -> False

showPath :: [Vertex] -> String
showPath vertices = concat $ intersperse "," $ fmap showVertex vertices
  where
    showVertex v = case v of
        Start -> "start"
        End -> "end"
        Cave c -> c

readGraph :: [String] -> Graph
readGraph lines = Graph $ do
    node <- split '-' <$> lines
    case node of
        ["start", str] -> [(Start, Cave str)]
        [str, "start"] -> [(Start, Cave str)]
        ["end", str] -> [(Cave str, End)]
        [str, "end"] -> [(Cave str, End)]
        [str1, str2] -> [(Cave str1, Cave str2), (Cave str2, Cave str1)]
        _ -> error ("invalid input: " <> show node)

split :: Eq a => a -> [a] -> [[a]]
split c xs =
    case findIndex (== c) xs of
        Just i ->
            [take i xs, drop (i + 1) xs]
        Nothing ->
            [xs]

getGraph :: IO Graph
getGraph = do
    ls <- lines <$> getContents
    return (readGraph ls)

main :: IO ()
main = do
    graph <- getGraph
    let ps = showPath <$> (paths Start End graph (const False) [])
    -- mapM_ print ps
    putStrLn ("Answer 1: " <> show (length ps))

    let ps2 = showPath <$> (paths Start End graph visitPermitted [])
    -- mapM_ print ps2
    putStrLn ("Answer 2: " <> show (length ps2))
