{-# LANGUAGE PartialTypeSignatures #-}
module Day2 where

import Data.IntMap.Strict
import qualified Data.IntMap.Strict as Map
import Control.Monad.Trans.State.Strict
import Control.Monad (forM_)
import Data.Foldable (find)

type Program = [Int]
type Mem = IntMap Int
type PC = Int
type Addr = Int

data Computer = Computer { pc :: PC, memory :: Mem } deriving (Show)

load :: Program -> Computer
load dat = Computer { pc = 0, memory = fromList (zip [0..] dat) }

deref :: Addr -> State Computer Int
deref addr = do
    mem <- gets memory
    pure (mem ! addr)

mov :: Int -> Addr -> State Computer ()
mov x addr =
    modify' $ \comp ->
        comp { memory = Map.insert addr x (memory comp) }

binop :: (Int -> Int -> Int) -> State Computer ()
binop f = do
    pc <- gets pc
    addr1 <- deref (pc + 1)
    addr2 <- deref (pc + 2)
    addr3 <- deref (pc + 3)
    arg1 <- deref addr1
    arg2 <- deref addr2
    mov (f arg1 arg2) addr3
    advance

add :: State Computer ()
add = binop (+)

mul :: State Computer ()
mul = binop (*)

advance :: State Computer ()
advance = modify' $ \comp -> comp { pc = pc comp + 4}

run :: State Computer ()
run = do
    Computer pc mem <- get
    case mem ! pc of
        1  -> add >> run
        2  -> mul >> run
        99 -> pure ()

run' :: Program -> Program
run' dat =
    case execState run (load dat) of
        Computer _ mem -> snd <$> Map.toList mem

main1 :: [Int] -> Int
main1 input = evalState program (load input)
  where
    program = do
        mov 12 0x1
        mov  2 0x2
        run
        deref 0x0

-- Part II --

type Noun = Int
type Verb = Int

runInput :: Program -> Noun -> Verb -> (Int, Noun, Verb)
runInput input noun verb = evalState (program noun verb) (load input)
  where
    program noun verb = do
        mov noun 0x1
        mov verb 0x2
        run
        r <- deref 0x0
        return (r, noun, verb)

main2 :: [Int] -> Int
main2 input =
    let Just (_, n, v) = find (\(r, _, _) -> r == 19690720) $ runInput input <$> [0..99] <*> [0..99]
    in 100 * n + v
