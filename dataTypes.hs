--data Boolean = False | True
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot

integral' :: Int -> Int -> Int
integral' x y = x + y
