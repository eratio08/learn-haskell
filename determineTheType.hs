{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType
    (
    ) where

example :: Num a => a
example = 1

a :: Num a => a
a = (* 9) $ 6

b :: Num a => (a,[Char])
b = head [(0, "doge"),(1,"Kitteh")]

c::(Integer,String)
c = head [(0::Integer, "dige"),(1,"Kitteh")]

d::Bool
d = if False
  then True
  else False

e::Int
e = length [1,2,3,4,5]
