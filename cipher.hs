module Cipher where
import Data.Char

caesar :: Int -> String -> String
caesar 0 str = str
caesar off str = map (chr . (+ mod off 26) . ord) str

unCaesar :: Int -> String -> String
unCaesar 0 str = str
unCaesar off str = map (chr . subtract (mod off 26) . ord) str

caesarIo :: IO ()
caesarIo = do
 putStrLn "Verschiebung eingeben "
 numberStr <- getLine
 putStrLn "Wort eingeben "
 wort <- getLine
 putStrLn (caesar (read numberStr) wort)

unCaesarIo :: IO ()
unCaesarIo = do
  putStrLn "Verschiebung eingeben"
  numberStr <- getLine
  putStrLn "Verschlüsseltes Wort eingeben"
  word <- getLine
  putStrLn (unCaesar (read numberStr) word)