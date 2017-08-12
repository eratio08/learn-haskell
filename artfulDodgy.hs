dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"
