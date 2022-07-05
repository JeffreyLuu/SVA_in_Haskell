module Assertion where

import Helper
import Token
import Keyword

data Assertion = AssertLine String deriving (Eq)

instance Show Assertion where
    show (AssertLine str) = "assert" ++ str

isAssertLine :: String -> Bool
isAssertLine s = (xs /= []) && (head xs == (Keyword Assert))
        where xs = tokenize s

getSingleAssertion :: String -> Assertion
getSingleAssertion s = AssertLine (removeCSPSign s)


getAssertions :: [String] -> [Assertion]
getAssertions [] = []
getAssertions (x:xs) | isAssertLine x = (getSingleAssertion x):remain
                     | otherwise = remain
        where remain = getAssertions xs

removeCSPSign :: String -> String
removeCSPSign str = if needRemove then l2 else error $ show str
    where needRemove = not (r == "")
          (l, r) = breakString "%-" str
          (l2, r2) = breakString "-%" $ drop 2 r


breakString :: String -> String -> (String, String)
breakString pattern str = breakString' pattern str ""

breakString' pattern "" cache = (reverse cache, "")
breakString' pattern str cache = if cond then (reverse cache, str) else other
    where cnt = length pattern
          cond = (take cnt str) == pattern
          other = breakString' pattern (tail str) ((head str):cache)