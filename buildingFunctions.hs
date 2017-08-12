module BuildingFunctions where

f1 :: [Char] -> [Char]
f1 xs = xs ++ "!"

f2 :: String -> Char
f2 xs = xs !! 4

f3 :: String -> String
f3 xs = drop 9 xs

thirdLetter :: String -> Char
thirdLetter xs = xs !! 2

letterIndex :: Int -> Char
letterIndex n = "Curry is awesome!" !! (n - 1)