module Test
    (
    ) where

data Trivial = Trivial'
instance Eq Trivial where (==) Trivial' Trivial' = True

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun deriving Show
data Date = Date DayOfWeek Int

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

instance Eq Date where
  (==) (Date weekday dayOfMonth)
       (Date weekday' dayOfMonth') =
         weekday == weekday' && dayOfMonth == dayOfMonth'

instance Ord DayOfWeek where
  compare Fri Fri = EQ
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

data Identity a = Identity a
instance Eq a =>  Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- exercise
data TisAsInteger = TisAn Integer
instance Eq TisAsInteger where
  (==) (TisAn int) (TisAn int') = int == int

data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  (==) (Two intOne intTwo)
       (Two intOne' intTwo')
       = intOne == intOne' && intTwo == intTwo'
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  (==) (TisAnInt int) (TisAnInt int') = int == int'
  (==) (TisAString str) (TisAString str') = str == str'
  (==) _ _ = False

data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair a a') (Pair b b') = a == b && a' == b'

data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a
instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne b) (ThatOne b') = b == b'
  (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

-- Exercise
data Mood = Blah
instance Show Mood where
  show _ = "Blah"
