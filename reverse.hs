module Reverse where
  
rvrs :: String -> String
rvrs xs = (take 7 $ drop 9 xs) ++ (take 4 $ drop 5 xs) ++ (take 5 xs)

main :: IO ()
main = print $ rvrs "Curry is awesome"