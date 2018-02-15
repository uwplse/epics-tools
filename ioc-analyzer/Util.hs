module Util where

import Data.Char (isSpace)

-- from http://en.wikipedia.org/wiki/Trim_(programming)#Haskell
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
