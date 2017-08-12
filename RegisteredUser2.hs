module RegisteredUser
    (
    ) where

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber number)) = putStrLn $ name ++ " " ++ show number

-- k (x,y) = x
f :: (a,b,c) -> (d,e,f) -> ((a,d),(c,f))
f (a,b,c) (d,e,f) = ((a,d),(c,f))

funcZ x =
  case x + 1 == 1 of
    True -> "AWESOME"
    False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal* xs =
  case y of
    True -> "yes"
    False -> "no"
  where
    y = xs == reverse xs
