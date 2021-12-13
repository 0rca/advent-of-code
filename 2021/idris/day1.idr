module Main

input1 : List Int
input1 = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

answer_1 : List Int -> Int
answer_1 list = go 0 list where
  go : Int -> List Int -> Int
  go acc (x0 :: x1 :: xs) =
    if x1 > x0 then go (acc + 1) (x1 :: xs)
    else go acc (x1 :: xs)
  go acc _ = acc

answer_2 : List Int -> Int
answer_2 list = go 0 list where
  go : Int -> List Int -> Int
  go acc (x0 :: x1 :: x2 :: x3 :: xs) =
    if x3 > x0 then go (acc + 1) (x1 :: x2 :: x3 :: xs)
    else go acc (x1 :: x2 :: x3 :: xs)
  go acc _ = acc

-- main : IO ()
-- main = do
--   putStrLn (show (answer_1 input1))
--   putStrLn (show (answer_2 input1))

getLines : IO (List String)
getLines = do
  l <- getLine
  if l == "" then pure [] else do
    ls <- getLines
    pure (l :: ls)


main : IO ()
main = do
  ls <- getLines
  print ls
