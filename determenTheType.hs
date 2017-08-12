module DetermineTheType
    (
    ) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

fib' :: Integer -> Integer
fib' n1 = fibiter 0 0 1 where
  fibiter n m1 m2 = if n1 == n then m1 else fibiter (n + 1) (m1 + m2) m1

bindExp :: Integer -> String
bindExp x = let y = 5 in
              "the integer was: " ++ show x
              ++ " and y was: " ++ show y

trip :: Integer -> Integer
trip = (\x -> x * 3)

mth = \x -> \y -> \z -> x * y * z


addIneIfOdd n = case odd n of
  True -> (\n -> n + 1) n
  False -> n

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
