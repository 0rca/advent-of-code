{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative
import Data.ByteString as BS
import Data.Attoparsec.ByteString.Char8
import Control.Monad (join)
import qualified Day1
import qualified Day2

pparse cont parser input =
    case parse parser input of
        Done _ r -> cont r
        Partial f -> case f "" of
            Done _ r -> cont r

day_1a = do
    input <- BS.readFile "input/day_01a.txt"
    pparse print (Day1.main1 <$> (decimal `sepBy` endOfLine)) input

day_1b = do
    input <- BS.readFile "input/day_01a.txt"
    pparse print (Day1.main2 <$> (decimal `sepBy` endOfLine)) input

day_2a = do
    input <- BS.readFile "input/day_02a.txt"
    pparse print (Day2.main1 <$> (decimal `sepBy` char ',')) input

day_2b = do
    input <- BS.readFile "input/day_02a.txt"
    pparse print (Day2.main2 <$> (decimal `sepBy` char ',')) input

opts =
    let cmds = [("day1a", day_1a)
               ,("day1b", day_1b)
               ,("day2a", day_2a)
               ,("day2b", day_2b)
               ]
    in subparser (foldMap (\(n,f) -> command n (info (pure f) idm)) cmds)

main :: IO ()
main = join $ execParser (info opts idm)
