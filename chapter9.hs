import Data.Char

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (a:[]) = Nothing
safeTail (_:xs) = Just xs

eftBool :: Bool -> Bool -> [Bool]
eftBool from to | from == to = [from]
                | otherwise = [from, to]

eftEnum :: (Enum a, Eq a, Ord a) => a -> a -> [a]
eftEnum from to | from == to = [from]
                | from < to = [from] ++ eftEnum (succ from) to
                | from > to = [from] ++ eftEnum (pred from) to

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftEnum
--
eftInt :: Int -> Int -> [Int]
eftInt = eftEnum

eftChar :: Char -> Char -> [Char]
eftChar = eftEnum

myWord :: String -> [String]
myWord [] = []
myWord (x:xs) | x == ' ' = myWord xs
myWord str = takeWhile (/=' ') str : myWord (dropWhile (/= ' ') str)

listcmpr = [ x^2 | x <- [1..10], rem x 2 == 0]


mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
myTuples = length [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (a:as) (b:bs) = f a b : myZipWith f as bs

myZip :: [a] -> [b] -> [(a,b)]
myZip = myZipWith (\a b -> (a,b))

capt :: String -> String
capt (x:xs) = toUpper x : xs

allCapt :: String -> String
allCapt = map toUpper

captHead :: String -> Char
captHead (x:xs) = toUpper x

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) | x == False = False
             | otherwise = x && myAnd xs

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) | x == True = True
            | otherwise = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) | f x == True = True
               | otherwise = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e lst = any (== e) lst

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] ->  [a]
squish [] = []
squish [[]] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xs = squishMap (++[]) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = go f xs x
  where
    go _ [] e = e
    go f (x:xs) e
      | f e x == EQ || f e x == GT = go f xs e
      | f e x == LT = go f xs x
