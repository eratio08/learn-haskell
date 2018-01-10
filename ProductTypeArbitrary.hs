module ProductTypeArbitrary where

import           Test.QuickCheck
import           Test.QuickCheck.Gen (oneof)

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do
  a <- arbitrary
  b <- arbitrary
  oneof [return $ First a, return $ First b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = frequency [(1, return Nothing), (3, fmap Just arbitrary)]
