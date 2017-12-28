module Palindrom where

import Control.Monad (forever)
import Data.Char (toLower)

ignoreCase :: String -> String
ignoreCase = fmap toLower

isPalindrom :: String -> Bool
isPalindrom s = ignoreCase s == reverse (ignoreCase s)

palindrom :: IO ()
palindrom = forever $ do
  line1 <- getLine
  case isPalindrom line1 of
    True -> putStrLn "It's a palindrom"
    False -> putStrLn "Nope"