module FirstMonoid where

import           Control.Monad
import           Data.Monoid
import           QuickCheckMonoid
import           Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = First' Nada
  mappend (First' Nada) (First' u) = First' u
  mappend (First' o) _             = First' o

instance Arbitrary a => Arbitrary (First' a) where
 arbitrary = genFirst'

genFirst' :: Arbitrary a => Gen (First' a)
genFirst'= do
  a <- arbitrary
  frequency [(1, return (First' Nada)), (3, return (First' (Only a)))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend =
  First' String -> First' String -> First' String -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
