module FirstMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import QuickCheckMonoid

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)

instance Monoid (First' a) where
  mempty = undefined
  mappend = undefined

instance Arbitrary First' where
 arbitrary = frequency [(1, return Fools), (1, return Twoo)]

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