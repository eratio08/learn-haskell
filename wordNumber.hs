module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"
  otherwise -> "unknown"

digits :: Int -> [Int]
digits n = go n []
  where
    go rest arr | rest <= 0 = arr
                       | otherwise = go (div rest 10) ((mod rest 10) : arr)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
