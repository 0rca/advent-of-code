{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Monoid
import System.Environment

-------------------------------------------------------------------------------
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
    let State{ counters } = go newState input
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

ans2 = undefined

main = do
    args <- getArgs
    case args of
        ["1"] ->
            print =<< ans1 <$> readInput
        ["2"] ->
            error "not implemented"
            -- print =<< ans2 <$> readInput
        _ ->
            print "Usage: cabal run day3 1|2 < input.txt"


