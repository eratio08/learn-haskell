module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
    where 
      gameLength w =
        let l = length (w :: String)
        in  l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = 
    (intersperse ' ' $ fmap renderPuzzleChar discovered) 
    ++ " Guessed so far " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle [] = Puzzle [] [] []
freshPuzzle s = Puzzle s (map (const Nothing ) s) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g 

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c =
  Puzzle word newFilledInSoFar (c : s)
  where
    zipper gussed wordChar guessChar =
      if wordChar == gussed
      then Just wordChar
      else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

main :: IO ()
main = do
  putStrLn "hello world"
