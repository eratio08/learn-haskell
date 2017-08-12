module Arith2
    (
    ) where

add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOnePF :: Int -> Int
addOnePF = addPF 1

main :: IO ()
main = do
  print (0 :: Int)
  print (add 1 0)
  print (addOnePF 0)
  print (addOnePF . addOnePF . addOnePF $ 2)
