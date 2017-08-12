module GreetIfCool1 where

greetIfCool :: String -> IO ()
greetIfCool coolness = 
  if cool
    then putStrLn "Hey"
  else
    putStrLn "..."
  where cool = coolness == "bla"

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]

isPalindrom :: (Eq a) => [a] -> Bool
isPalindrom x = x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else negate x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f xs ys = ((snd xs, snd ys),(fst xs, fst ys))
