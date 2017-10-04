{-# LANGUAGE FlexibleInstances #-}
import Data.Int
import Data.Char

-- Rule the types rule the universe
data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Length deriving (Eq, Show)
type Length = Integer

-- Exercises: Vehicles
myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. -> Vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars x = map isCar x

-- 3.
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
getManu _ = undefined

-- 4. undefined
-- 5. done

-- nullary
data Example0 = Example0 deriving (Eq, Show)
-- unary
data Example1 = Example1 Int deriving (Eq, Show)
-- product of Int and String
data Example2 = Example2 Int String deriving (Eq, Show)

-- Excercise: Cardinality
-- 1. one
-- 2. three
-- 3. Int8 = 256, Int16 = 65536
-- 4. Int = 2^64 (on 64bit)
-- 5. 2^8 = 256

-- Exercise: For Example
data Example = MakeExample deriving Show
data MyExample = MakeMyExample Int deriving (Show)
-- Unary Constructors
newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

-- 11.9 newtype
-- record type = product type
-- tagged union = sum type

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

instance TooMany Goats where
  tooMany (Goats n) = n > 43

-- Exercise: Lofic Goats
-- 1.
instance TooMany (Int, String) where
  tooMany (n, str) = tooMany n

instance TooMany (Int, Int) where
  tooMany (a,b) = tooMany (a + b)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany (a, b) = tooMany (a + b)

-- SSum types
-- Exercises: Pity the Bool
-- 1.
data BigSmall = Big Bool | Small Bool deriving (Eq, Show)
-- Cardinality = 4
-- 2.
data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
--
myNumba = Numba (-127)
-- 256 + 2

-- 11.11 Product types
data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)
--data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)
type TwoQs = (QuantumBool, QuantumBool)

-- Record syntax
data Person' = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 108
ca = MkPerson "chris" 16

namae :: Person' -> String
namae (MkPerson s _) = s

data Person = Person { name :: String
                     , age :: Int} deriving (Eq, Show)

-- 11.12 Normal form
-- Distriputiv Gesetzt
-- a * (b + c) -> (a * b) + (a * c)
--data Fiction = Fiction deriving Show
--data Nonfiction = Nonfiction deriving Show
--data BookType = FictionBook Fiction | NonfictionBook Nonfiction deriving Show
type AuthorName = String
--data Author = Author (AuthorName, BookType)
data Author = Fiction AuthorName | Nonfiction AuthorName
data Expr =
   Number Int
 | Add Expr Expr
 | Minus Expr
 | Mult Expr Expr
 | Divide Expr Expr

-- Exercise: How Does Your Garden Grow?
-- 1.
--data FlowerType = Gardenia
--                  | Daisy
--                  | Rose
--                  | Lilac
--                  deriving (Show)
type Gardener = String
--data Garden = Garden Gardener FlowerType deriving Show

data Garden = Gardenia Gardener | Daisy Gardener | Rose Gardener | Lilac Gardener deriving Show

data Sum a b = First a | Second b deriving (Show)

data Test = Test String Int deriving (Show)

-- Exercises: Programmers
data OperatingSystem = GnuPlusLinux
                       | OpenBSDPlusNevermindJustBSDStill
                       | Mac
                       | Windows
                       deriving (Eq, Show)

data ProgLang = Haskell
                | Agda
                | Idris
                | PureScript
                deriving (Eq, Show)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgLang }
                             deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
                [ GnuPlusLinux
                , OpenBSDPlusNevermindJustBSDStill
                , Mac
                , Windows
                ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [(Programmer os lang) | os <- allOperatingSystems, lang <- allLanguages ]


-- data ThereYet = There Float Int Bool deriving (Eq, Show)
-- -- who needs a "builder pattern"?
-- notYet :: Int -> Bool -> ThereYet
-- notYet = nope 25.5
-- notQuite :: Bool -> ThereYet
-- notQuite = notYet 10
-- yusssss :: ThereYet
-- yusssss = notQuite False

newtype Name = Name String deriving (Show)
newtype Acres = Acres Int deriving (Show)
data FarmerType = DairyFarmer
                  | WheatFarmer
                  | SoybeanFarmer
                  deriving (Show)
data Farmer = Farmer Name Acres FarmerType

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False
-- Recordtype also possible

-- Exercises: The Quad
-- 1
-- 4 Possible varriations for the Type
data Quad = One | Two | Three | Four deriving (Eq, Show)
eQuad :: Either Quad Quad
eQuad = Left One
-- 2 * 4
-- 2.
prodQuad :: (Quad, Quad)
prodQuad = (One, One)
-- 4 * 4
-- 3.
funcQuad :: Quad -> Quad
funcQuad One = One
-- 4^4
prodTBool :: (Bool, Bool, Bool)
prodTBool = (True, True, True)
-- 2 * 2 * 2
gTwo :: Bool -> Bool -> Bool
gTwo _ _ = True
-- 2 ^ 2 ^ 2 = 2 ^ 4
fTwo :: Bool -> Quad -> Quad
fTwo _ _ = One
-- 4 ^ 4 ^ 2 = 4 ^ 8

-- Kinds
data Silly a b c d = MkSilly a b c d deriving (Show)

data List a = Empty | Cons a (List a)

-- 11.17 Binary Tree
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right) | b == a = Node left a right
                              | b < a = Node (insert' b left) a right
                              | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)
-- acceptance test for mapTree
mapOkay =
  if mapTree (+1) testTree' == mapExpected
   then print "yup okay!"
   else error "test failed!"

-- Tree to List
preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder = reverse . preorder

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

-- foldr for BinaryTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = f a (foldTree f (foldTree f acc right) left)

repCypher :: String -> String
repCypher = foldr (++) [] . repeat

-- encryptChar :: Char -> Char -> Char
-- encryptChar c cyph = chr (ord c - offset cyph) where
--   offset = (65-) . ord

-- encryptChar :: Char -> String -> Int -> Char
-- encryptChar ' ' _ = ' '
-- encryptChar c cyph n = chr (offset c) where
--   takeCypher = (cyph !!) . (flip mod) (length cyph)
--   offset x n = ord x - ord (takeCypher n)

encryptChar:: Char -> Char -> Char
encryptChar ' ' _ = ' '
encryptChar s c = chr (65 + (mod (((ord s) - 65) + ((ord c) - 65)) 26))

-- takeCypher :: String -> Char
-- takeCypher cyp = (cyp !!) . (flip mod) (length cyp)

crypt :: String -> String -> String
crypt [] _ = []
crypt str [] = str
crypt (' ':xs) (y:ys) = ' ' : crypt xs (y:ys)
crypt (x:xs) (y:ys) = (encryptChar x y) : (crypt xs ys)

vingenereCipher :: String -> String -> String
vingenereCipher [] _ = []
vingenereCipher str [] = str
vingenereCipher str cyp = crypt str (take (length str) (repCypher cyp))

-- As-pattern
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
  print a
  return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:xy) = x : xs ++ xy

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf seq1@(x:xs) seq2@(y:ys) = charInSeq x seq2 && isSubseqOf xs seq2 where
  charInSeq :: Eq a => a -> [a] -> Bool
  charInSeq _ [] = False
  charInSeq c (x:xs) = c == x || charInSeq c xs --geht auch mit fold

capitalizeWords :: String -> [(String, String)]
capitalizeWords = cap . words where
  cap :: [String] -> [(String, String)]
  cap [] = []
  cap (x:xs) = (x, map toUpper x) : cap xs

-- Language Exercise
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = unwords . (map capitalizeWord) . words
