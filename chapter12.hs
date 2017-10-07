import Data.Char
-- 12.1 Signaling adversity
data Maybe' a = Nothing' | Just' a

type Name = String
type Age = Integer
data Person = Person Name Age deriving Show

-- this is a smart constructor
mkPerson :: Name -> Age -> Maybe Person
mkPerson name age
  | name /= "" && age >= 0 =
    Just $ Person name age
  | otherwise = Nothing

-- 12.3 Bleating either
data Either' a b = Left' a | Right' b

data PersonInvalid = NameEmpty | AgeTooLow deriving (Eq, Show)

mkPerson' :: Name -> Age -> Either PersonInvalid Person
mkPerson' name age
  | name /= "" && age >= 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | otherwise = Left AgeTooLow

type ValidatePerson a = Either [PersonInvalid] a

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age >= 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

mkPersonA :: Name -> Age -> ValidatePerson Person
mkPersonA name age = mkPersonB (nameOkay name) (ageOkay age)

mkPersonB :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPersonB (Right nameOkay) (Right ageOkay) = Right (Person nameOkay ageOkay)
mkPersonB (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPersonB (Left badName) _ = Left badName
mkPersonB _ (Left badAge) = Left badAge


-- 12.4
data Example a = Blah | RoofGoats | Woot a deriving Show

-- String Processing
notThe :: String -> Maybe String
notThe s = case s == "the" of
  True -> Nothing
  False -> Just s

replaceThe :: String -> String
replaceThe = unwords . replaceThe' . fmap notThe . words
  where 
    replaceThe' [] = []
    replaceThe' (Nothing:xs) = ["a"] ++ replaceThe' xs
    replaceThe' ((Just s):xs) = [s] ++ replaceThe' xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = count . words
    where 
      count [] = 0
      count (x:(y:ys):z) 
        | x == "the" && (y == 'a' || y == 'e' || y == 'i' || y == 'o' || y == 'u') = 1 + count z
      count (_:z) = count z

isVowel :: Char -> Bool
isVowel s = case toLower s of
  'a' -> True
  'e' -> True
  'i' -> True
  'o' -> True
  'u' -> True
  _ -> False

getVowels :: String -> [Char]
getVowels = filter isVowel

countVowels :: String -> Integer
countVowels = toInteger . length . getVowels

-- Validate the word
newtype Word' = Word' { unWord' :: String } deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

isVowel' :: Char -> Bool
isVowel' x = foldr (\e acc -> if e == x then acc || True else acc || False) False vowels

countVowels' :: String -> Int
countVowels' = length . filter isVowel'

mkWord :: String -> Maybe Word'
mkWord s = case (div (length s) 2) >= (countVowels' s) of
  True -> Just (Word' s)
  _  -> Nothing

-- It's only natural
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat i | i < 0 = Nothing
integerToNat i = Just . integerToNat' $ i
  where
    integerToNat' :: Integer -> Nat
    integerToNat' 0 = Zero
    integerToNat' i = Succ . integerToNat' . flip (-) 1 $ i

-- Small Library for Maybe
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b f Nothing = b
mayybee b f (Just a) = f a

fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe a (Just b) = b

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes = fmap (\(Just x) -> x) . filter isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Nothing
flipMaybe x | (length . filter isNothing $ x) > 0 = Nothing
flipMaybe x = Just . catMaybes $ x

-- Small library for Either
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

lefts' :: [Either a b] -> [a]
lefts' = foldr (\(Left e) acc -> e : acc) [] . filter isLeft

rights' :: [Either a b] -> [b]
rights' = foldr (\(Right e) acc -> e : acc) [] . filter isRight

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = (lefts' xs, rights' xs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left x) = f x
either' f g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f xs = either' (\ x -> Nothing)  (Just . f)  xs