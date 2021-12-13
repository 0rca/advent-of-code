module Main where

import Control.Monad (guard)
import qualified Data.Char as Char
import Data.List (findIndex, intersperse)

data Vertex = Start | End | Cave String deriving (Eq, Show)
type Edge = (Vertex, Vertex)
newtype Graph = Graph {unGraph :: [Edge]} deriving (Show)

canRevisit :: Vertex -> Bool
canRevisit (Cave c) = all Char.isUpper c
canRevisit _ = False

paths :: Vertex -> Vertex -> Graph -> [Vertex] -> [[Vertex]]
paths a b (Graph edges) visited = do
    (c, d) <- edges
    guard (a == c)
    guard (canRevisit d || d `notElem` visited)
    if b == d
        then return (reverse (d : c : visited))
        else paths d b (Graph edges) (c : visited)

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
    let ps = showPath <$> (paths Start End graph [])
    -- mapM_ print ps
    putStrLn ("Total paths: " <> show (length ps))
