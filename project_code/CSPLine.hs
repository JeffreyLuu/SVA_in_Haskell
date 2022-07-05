module CSPLine where

import Helper
import Token

getCSPLine :: String -> String
getCSPLine s = drop 2 s

isCSPStatement :: Tokentype -> Bool
isCSPStatement (CSPStatement _) = True
isCSPStatement _ = False

isCSPLine :: String -> Bool
isCSPLine s = (xs /= []) && (isCSPStatement $ head xs)
        where xs = tokenize s