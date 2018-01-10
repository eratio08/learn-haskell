module UsingQuickCheck where

import           Data.List
import           Test.QuickCheck

half :: (Fractional a) => a -> a
half x = x / 2

halfIdentity :: (Fractional a) => a -> a
halfIdentity = (*2) . half

testHalf :: (Eq a, Fractional a) => a -> Bool
testHalf x = x == halfIdentity x

type CheckDouble = Double -> Bool
type CheckFloat = Float -> Bool

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

checkSorted :: Ord a => [a] -> Bool
checkSorted = listOrdered . sort

type CheckInts = [Int] -> Bool
type CheckInt3 = Int -> Int -> Int -> Bool
type CheckInt2 = Int -> Int -> Bool
type CheckFunc = (Int -> Int) -> Int

plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + ( y + z ) == (x + y ) + z

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y = x * y == y * x

quotRem' :: (Integral a, Real a, Enum a) => a -> a -> Bool
quotRem' _ 0 = True
quotRem' x y = (quot x y) * y + (rem x y) == x

divMod' :: (Integral a, Real a, Enum a) => a -> a -> Bool
divMod' _ 0 = True
divMod' x y = (div x y) * y + (mod x y) == x

powerAssoc :: (Num a, Integral a) => a -> a -> a -> Bool
powerAssoc x y z = x ^ ( y ^ z ) == ( x ^ y ) ^ z

powerCommu :: (Num a, Integral a) => a -> a -> Bool
powerCommu x y = x ^ y == y ^ x

listIdentity :: (Ord a) => [a] -> Bool
listIdentity xs = xs == (reverse . reverse $ xs)

checkApply ::(Eq a, Eq b) => (a -> b) -> a -> Bool
checkApply f a = (f $ a) == f a

main :: IO ()
main = do
  quickCheck (testHalf :: CheckDouble)
  quickCheck (testHalf :: CheckFloat)
  quickCheck (checkSorted :: CheckInts)
  quickCheck (plusAssociative :: CheckInt3)
  quickCheck (plusCommutative :: CheckInt2)
  quickCheck (multAssociative :: CheckInt3)
  quickCheck (multCommutative :: CheckInt2)
  quickCheck (quotRem' :: CheckInt2)
  quickCheck (divMod' :: CheckInt2)
  quickCheck (powerAssoc :: CheckInt3)
  quickCheck (powerCommu :: CheckInt2)
  quickCheck (listIdentity :: CheckInts)
  quickCheck (checkApply id :: CheckFunc)
