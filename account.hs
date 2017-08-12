data Person = Person {firstName::String, lastName::String} deriving (Show)
data Account = Account {owner::Person, balance::Double} deriving (Show)
data Transaction = Transaction {amount::Double, from::Account, to::Account}

person1 = Person "Eike" "Lurz"
balance1 = 1000.0
account1 = Account person1 balance1
account3 = Account (Person "Test" "Person") balance1

addBalance :: Double -> Account -> Account
addBalance amount (Account owner balance) = Account owner (balance + amount)

add500 = addBalance 500

reduceBalance :: Double -> Account -> Account
reduceBalance amount (Account owner balance) = Account owner (balance - amount)

account2 = reduceBalance 1000
           . addBalance 500
           . addBalance 500
           . reduceBalance 500
           $ account1

doTransaction :: Transaction -> (Account, Account)
doTransaction (Transaction amount0 (Account owner1 amount1) (Account owner2 amount2)) = ((Account owner1 (amount1 - amount0)), (Account owner2 (amount2 + amount0)))

transaction = Transaction 500 account1 account3

test = doTransaction transaction

add500Test = add500 . add500 $ account1
