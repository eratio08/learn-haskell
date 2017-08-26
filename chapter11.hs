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
