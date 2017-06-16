module Cipher where

import Data.Char

--chr ord

-- Caeser cipher which simply shifts letters in the alphabet by a fixed number to the right
caesar :: String -> Int -> String
caesar [] _ = []
caesar (x:xs) n = caesarChar x n : caesar xs n

caesarChar :: Char -> Int -> Char
caesarChar c n = chr $ (97+) $ mod ((ord c) - 97 + n) 26

unCaesar :: String -> Int -> String
unCaesar [] _ = []
unCaesar (x:xs) n = caesarChar x iNv : unCaesar xs n
  where iNv = negate n
