module Chapter16 where

replaceWithP :: b -> Char
replaceWithP = const 'b'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]] -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f) => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted = (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]] -> [Maybe [Char]]
thriceLifted' = thriceLifted

main :: IO ()
main = do
  print lms

  putStr "replaceWithP' lms:"
  print (replaceWithP' lms)

  putStr "liftedReplace lms:"
  print (liftedReplace lms)

  putStr "liftedReplace' lms:"
  print (liftedReplace' lms)

  putStr "twiceLifted lms:"
  print (twiceLifted lms)

  putStr "twiceLifted' lms:"
  print (twiceLifted' lms)

  putStr "thriceLifted lms:"
  print (thriceLifted lms)

  putStr "thriceLifted' lms:"
  print (thriceLifted' lms)

-- Exercise: Heavy lifting
-- 1.
a :: [Int]
a = fmap (+ 1) $ read "[1]" :: [Int]

-- 2.
b :: Maybe [[Char]]
b = (fmap . fmap) (++ "lol") (Just ["Hi", "Hello"])

-- 3.
c :: Integer -> Integer
c = (* 2) . (\x -> x - 2)

-- 4.
d :: Integer -> [Char]
d = ((return '1' ++) . show) . (\x -> [x, 1 .. 3])

-- 5.
e :: IO Integer
e =
  let ioi = readIO "1" :: IO Integer
      changed = fmap (read . ("123" ++) . show) ioi
   in fmap (* 3) changed