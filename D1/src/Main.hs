module Main where

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n lst = let 
  t = tail lst 
  h = take n lst
  in if length t < n then h : chunk n [] else h : chunk n t

sumOfChunk :: [String] -> Int
sumOfChunk = sum . map (\str -> read str :: Int)

calc' :: Int -> [String] -> Int
calc' value [] = value
calc' value (x:xs) =
  case xs of
    [] -> calc' value xs
    _ ->
      let
        c = read x :: Int
        n = read (head xs) :: Int
        decAmount = if c < n then value + 1 else value
      in calc' decAmount xs

calc'' :: Int -> [Int] -> Int
calc'' value [] = value
calc'' value (x:xs) =
  case xs of
    [] -> calc'' value []
    _ ->
      let
        decAmount = if x < head xs then value + 1 else value
      in calc'' decAmount xs

main :: IO ()
main = do
  content <- readFile "/Users/zenobio/Projects/Etc/Haskell/aoc2021/D1/src/input.txt"
  (print . calc' 0 . lines) content
  (print . calc'' 0 . map sumOfChunk . chunk 3 . lines) content