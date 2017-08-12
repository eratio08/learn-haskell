fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
-- applyTimes n f b = f (applyTimes (n - 1) f b)
applyTimes n f b = f . applyTimes (n - 1) f $ b


-- data Maybe' a = Nothing' | Just' a deriving (Show)
f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

-- type Numerator = Integer
-- type Denominator = Integer
-- type Quotient = Integer
--
-- dividdeBy :: Numerator -> Denominator -> Quotient
-- dividedBy = div

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 n | n > 100 = n - 10
       | n <= 100 = mc91 (mc91 (n + 11))
