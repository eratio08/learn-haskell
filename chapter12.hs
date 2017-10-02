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