module ItentityCrisis where

import           Test.QuickCheck

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

data Test = Test deriving (Show, Eq)

