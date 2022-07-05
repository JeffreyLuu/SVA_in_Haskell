module Helper where

import Data.Maybe
import Text.Read

-- shared data type

type VarName = String
type ProcName = String
data Errors = VError | Error deriving (Show, Eq)


-- functions

tail' :: [a] -> [a]
tail' xs = if (length xs == 0) then xs else tail xs


split :: (Eq a) => [a] -> (a -> Bool) -> [[a]]
split xs fn = reverse $ split' xs fn [] []
split' (x:xs) fn curr cache | (fn x) = if (curr == []) then split' xs fn [] cache else split' xs fn [] ((reverse curr):cache)
                            | otherwise = split' xs fn (x:curr) cache
split' [] _ curr cache = if (curr == []) then cache else (reverse curr):cache


strToInt :: String -> Int
strToInt s = if (isJust res) then (fromJust res) else error "Invalid Integer"
        where res = readMaybe s :: Maybe Int

matchIdx :: (Eq a) => [a] -> [a] -> Int
matchIdx xs ls = matchIdx' xs ls 0

matchIdx' (x:xs) ls n = if (elem x ls) then n else 1+(matchIdx' xs ls n)
matchIdx' [] _ n = n

splitAtFirst :: (Eq a) => a -> [a] -> ([a], [a])
splitAtFirst x = fmap (drop 1) . break (x ==)

topLvlBreak :: (Eq a) => (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> [a] -> ([a], [a])
topLvlBreak l r bk xs = topLvlBreak' l r bk 0 xs

topLvlBreak' l r bk n (x:xs) | (l x) = tupleLeftJoin x $ topLvlBreak' l r bk (n+1) xs
                             | (r x) = tupleLeftJoin x $ topLvlBreak' l r bk (n-1) xs
                             | ((bk x) && (n == 0)) = ([], xs)
                             | otherwise = tupleLeftJoin x $ topLvlBreak' l r bk n xs
topLvlBreak' l r bk n [] = if n == 0 then ([], []) else error "cannot pair up"


tupleLeftJoin :: a -> ([a], [a]) -> ([a], [a])
tupleLeftJoin x (l, r) = (x:l, r)

topLvlSplit :: (Eq a) => (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> [a] -> [[a]]
topLvlSplit l r bk [] = []
topLvlSplit l r bk xs = if right == [] then [left] else (left):(topLvlSplit l r bk right)
    where (left, right) = topLvlBreak l r bk xs

topLvlElem :: (Eq a) => (a -> Bool) -> (a -> Bool) -> (a -> Bool) -> [a] -> Bool
topLvlElem l r bk xs = topLvlElem' l r bk 0 xs

topLvlElem' l r bk n (x:xs) | (l x) = topLvlElem' l r bk (n+1) xs
                             | (r x) = topLvlElem' l r bk (n-1) xs
                             | ((bk x) && (n == 0)) = True
                             | otherwise = topLvlElem' l r bk n xs
topLvlElem' l r bk n [] = if n == 0 then False else error "cannot pair up"

removeDuplicate :: (Eq a) => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (x:xs) | elem x xs = removeDuplicate xs
                       | otherwise = x:(removeDuplicate xs)

join :: String -> [String] -> String
join a [] = ""
join a [x] = x
join a (x:xs) = x ++ a ++ (join a xs)

getMinElem :: (a -> Int) -> [a] -> a
getMinElem cmp [] = error "have no element to compare"
getMinElem cmp (x:[]) = x
getMinElem cmp (x:y:xs) = if (cmp x) <= (cmp y) then getMinElem cmp (x:xs) else getMinElem cmp (y:xs)

