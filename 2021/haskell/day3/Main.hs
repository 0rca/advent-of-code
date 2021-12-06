{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Foldable (foldl')
import qualified Data.Foldable as Foldable
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid
import System.Environment

------------------- Part I ----------------------------------------------------
-- Direct implementation without too many abstractions
--

data Bin = Bin {n_zero :: Sum Int, n_one :: Sum Int} deriving (Show)

instance Semigroup Bin where
    b1 <> b2 = Bin{n_zero = n_zero b1 <> n_zero b2, n_one = n_one b1 <> n_one b2}

instance Monoid Bin where
    mempty = Bin mempty mempty

data State = State
    { current_bit :: !Int
    , counters :: IntMap Bin
    }
    deriving (Show)

newState = State 0 IntMap.empty

readInput = getContents

ans1 input =
    let State{counters} = go newState input
        (maxPower, _) = IntMap.findMax counters
        (gamma, epsilon) = rates maxPower counters
     in gamma * epsilon
  where
    go st@State{current_bit, counters} ('0' : xs) =
        go
            st
                { current_bit = current_bit + 1
                , counters = IntMap.insertWith (<>) current_bit (Bin 1 0) counters
                }
            xs
    go st@State{current_bit, counters} ('1' : xs) =
        go
            st
                { current_bit = current_bit + 1
                , counters = IntMap.insertWith (<>) current_bit (Bin 0 1) counters
                }
            xs
    go st@State{current_bit, counters} ('\n' : xs) =
        go st{current_bit = 0} xs
    go st [] = st

rates :: Int -> IntMap Bin -> (Int, Int)
rates maxPower counters =
    foldl buildRate (0, 0) (IntMap.toList counters)
  where
    buildRate (gamma, epsilon) (power, bin) =
        if n_one bin > n_zero bin
            then (gamma + 2 ^ (maxPower - power), epsilon)
            else (gamma, epsilon + 2 ^ (maxPower - power))

-------------------------- Part II --------------------------------------------

data Binary where
    O :: Binary
    I :: Binary
    (:>) :: Binary -> Binary -> Binary
    deriving (Eq, Show)

infixr 5 :>

data BinTree where
    X :: Binary -> BinTree
    L :: Binary -> BinTree -> BinTree
    Y :: BinTree -> BinTree -> BinTree
    deriving (Eq, Show)

binaryToTree :: Binary -> BinTree
binaryToTree O = X O
binaryToTree I = X I
binaryToTree (I :> bin) = L I (binaryToTree bin)
binaryToTree (O :> bin) = L O (binaryToTree bin)

instance Semigroup BinTree where
    X x0 <> X x1
        | x0 == x1 = X x0
        | otherwise = Y (X O) (X I)
    L x0 t0 <> L x1 t1
        | x0 == x1 = L x0 (t0 <> t1)
        | x0 == O = Y (L x0 t0) (L x1 t1)
        | otherwise = Y (L x1 t1) (L x0 t0)
    Y t0 t1 <> Y t2 t3 = Y (t0 <> t2) (t1 <> t3)
    Y t0 t1 <> L O t2 = Y (L O t2 <> t0) t1
    Y t0 t1 <> L I t2 = Y t0 (L I t2 <> t1)
    l@(L _ _) <> r@(Y _ _) = r <> l
    a <> b =
        error ("unhandled input: " <> show a <> " <> " <> show b)

strToBinary :: String -> Binary
strToBinary ['1'] = I
strToBinary ['0'] = O
strToBinary ('0' : xs) = O :> strToBinary xs
strToBinary ('1' : xs) = I :> strToBinary xs

mkTree input = foldr1 (<>) (fmap (binaryToTree . strToBinary) input)

treeLen :: BinTree -> Int
treeLen (X _) = 1
treeLen (L _ t) = treeLen t
treeLen (Y l r) = treeLen l + treeLen r

ppr :: Int -> BinTree -> IO ()
ppr i (X x) = putStrLn (replicate i ' ' <> show x)
ppr i (L x t) = putStrLn (replicate i ' ' <> show x) >> ppr (i + 1) t
ppr i (Y l r) = do
    ppr i l
    ppr i r

oxyRating tree = binToInt (go [] tree)
  where
    go bin (Y l r) =
        if treeLen r >= treeLen l
            then go bin r
            else go bin l
    go bin (L b t)
        | b == I = go (1 : bin) t
        | otherwise = go (0 : bin) t
    go bin (X b)
        | b == I = 1 : bin
        | otherwise = 0 : bin

co2Rating tree = binToInt (go [] tree)
  where
    go bin (Y l r) =
        if treeLen r >= treeLen l
            then go bin l
            else go bin r
    go bin (L b t) =
        case b of
            I -> go (1 : bin) t
            O -> go (0 : bin) t
    go bin (X b) =
        case b of
            I -> 1 : bin
            O -> 0 : bin

binToInt xs = go 0 xs
  where
    go pow (x : xs) = x * 2 ^ pow + go (pow + 1) xs
    go _ [] = 0

readInput2 = lines <$> getContents

readInput3 :: IO [[Int]]
readInput3 = do
    ls <- lines <$> getContents
    return (fmap (fmap toInt) ls)

ans2 :: [String] -> Int
ans2 input =
    let tree = mkTree input
     in (oxyRating tree * co2Rating tree)

ans3 :: [[Int]] -> Int
ans3 input =
    let ox = oxy_rate [] input
        co = co_rate [] input
     in (binToInt ox * binToInt co)
main = do
    args <- getArgs
    case args of
        ["1"] ->
            print =<< ans1 <$> readInput
        ["2"] ->
            print =<< ans2 <$> readInput2
        ["2a"] ->
            print =<< ans3 <$> readInput3
        _ ->
            print "Usage: cabal run day3 1|2|2a < input.txt"

input =
    [ "00100"
    , "11110"
    , "10110"
    , "10111"
    , "10101"
    , "01111"
    , "00111"
    , "11100"
    , "10000"
    , "11001"
    , "00010"
    , "01010"
    ]

test =
    and
        [ length input == treeLen (mkTree input)
        , ans2 input == 230
        ]

------------------------- Simpler take on the Day 3: No Trees ------------------

-- >>> reverse (oxy_rate [] input)
-- [1,0,1,1,1]

-- transpose :: [[a]] -> [_]
oxy_rate bin [] = bin
oxy_rate bin [rem] = foldl (flip (:)) bin rem
oxy_rate bin xs =
    let hs = heads xs
        filteredBy pred xs = map snd $ filter (\(counters, _) -> pred counters > 0) xs
     in case hs of
            [] ->
                bin
            _ ->
                case foldMap fst hs of
                    Bin z o ->
                        if o >= z
                            then oxy_rate (1 : bin) (filteredBy n_one hs)
                            else oxy_rate (0 : bin) (filteredBy n_zero hs)

co_rate bin [] = bin
co_rate bin [rem] = foldl (flip (:)) bin rem
co_rate bin xs =
    let hs = heads xs
        filteredBy pred xs = map snd $ filter (\(counters, _) -> pred counters > 0) xs
     in case hs of
            [] ->
                bin
            _ ->
                case foldMap fst hs of
                    Bin z o ->
                        if z <= o
                            then co_rate (0 : bin) (filteredBy n_zero hs)
                            else co_rate (1 : bin) (filteredBy n_one hs)

heads :: [[Int]] -> [(Bin, [Int])]
heads xs = do
    l <- xs
    case l of
        (0 : t) -> return (Bin 1 0, t)
        (1 : t) -> return (Bin 0 1, t)
        _ -> []

toInt '0' = 0
toInt '1' = 1
toInt x = error ("invalid input: " <> show x)
