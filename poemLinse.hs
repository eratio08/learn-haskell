module PoemLines where
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen
            ++ thirdSen ++ fourthSen

-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
seperateFor :: String -> Char -> [String]
seperateFor [] _ = []
seperateFor (x:xs) cha | x == cha = seperateFor xs cha
seperateFor str cha = takeWhile (/= cha) str : seperateFor (dropWhile (/= cha) str) cha

myLines :: String -> [String]
myLines str = seperateFor str '\n'


-- What we want 'myLines sentences' to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?" ]
-- The main function here is a small test -- to ensure you've written your function -- correctly.
main :: IO ()
main = print $ "Are they equal? " ++ show (myLines sentences == shouldEqual)
