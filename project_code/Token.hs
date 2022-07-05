module Token where

import Helper
import Keyword

import Control.Exception
import Data.List
import Data.Char


-- data type
data Tokentype = Value String | Special String | Empty | Keyword Keyword | CSPStatement String
        deriving (Eq)

instance Show Tokentype where
        show (Value s) = show s
        show (Special s) = show s
        show (Empty) = show ""
        show (Keyword k) = show k
        show (CSPStatement s) = show s


-- functions

leftParenthesis = [(Special "{"), Special "[", Special "("]
rightParenthesis = [(Special "}"), Special "]", Special ")"]

topLvlParenthesisElem :: (Tokentype -> Bool) -> [Tokentype] -> Bool
topLvlParenthesisElem bk = topLvlElem (\x -> elem x leftParenthesis) (\x -> elem x rightParenthesis) bk


topLvlParenthesisBreak :: (Tokentype -> Bool) -> [Tokentype] -> ([Tokentype], [Tokentype])
topLvlParenthesisBreak bk = topLvlBreak (\x -> elem x leftParenthesis) (\x -> elem x rightParenthesis) bk

topLvlParenthesisSplit :: (Tokentype -> Bool) -> [Tokentype] -> [[Tokentype]]
topLvlParenthesisSplit bk = topLvlSplit (\x -> elem x leftParenthesis) (\x -> elem x rightParenthesis) bk

getTokenValue :: Tokentype -> String
getTokenValue (Value s) = s
getTokenValue x = error $ "Invalid Tokentype " ++ (show x) ++ "!"


getKeyword2 :: Tokentype -> Tokentype
getKeyword2 (Value x) = if (getKeyword x /= Null) then (Keyword (getKeyword x)) else (Value x)
getKeyword2 x = x

mapGetKeyword :: [Tokentype] -> [Tokentype]
mapGetKeyword = map getKeyword2


-- tokenize is the main function to be used elsewhere

tokenize :: String -> [Tokentype]
-- tokenize s = filter (/= Empty) $ tokenize' s
tokenize s = mapGetKeyword $ removeComments $ filter (/= Empty) $ recombine $ iterCombineSpecialToken $ naiveTokenSingleLine s

tokenize' s = mapGetKeyword $ removeComments $ recombine $ iterCombineSpecialToken $ naiveTokenSingleLine s

isSpecialToken :: Tokentype -> Bool
isSpecialToken (Special _) = True
isSpecialToken _ = False

isValueToken :: Tokentype -> Bool
isValueToken (Value _) = True
isValueToken _ = False


getSingleVarName :: [Tokentype] -> VarName
getSingleVarName xs = if length xs /= 1 then error (show xs) else getTokenValue (head xs) -- assert (length xs == 1)



combineToken :: Char -> Tokentype -> Tokentype
combineToken c (Value str) = Value (c:str)
combineToken c (Special str) = Special (c:str)
combineToken c Empty = Empty
combineToken _ _ = error "Invalid token to be combined with"


concateStr :: [Tokentype] -> String
concateStr [] = ""
concateStr ((Value w):xs) = w++(concateStr xs)
concateStr ((Special w):xs) = w++(concateStr xs)
concateStr (Empty:xs) = " "++(concateStr xs)
concateStr ((Keyword x):xs) = (show x)++(concateStr xs)



recombine :: [Tokentype] -> [Tokentype]
recombine [] = []
recombine ((Special "%%"):xs) = [CSPStatement (concateStr xs)]
recombine ((Special "%-"):xs) = (CSPStatement (concateStr l)):(recombine $ tail r)
          where (l, r) = break ((Special "-%") == ) xs
recombine (x:xs) = x:(recombine xs)


removeComments :: [Tokentype] -> [Tokentype]
removeComments ((Special "//"):xs) = removeComments' xs
removeComments ((Special "\n"):xs) = removeComments xs
removeComments (x:xs) = x:(removeComments xs)
removeComments [] = [] 

removeComments' ((Special "\n"):xs) = removeComments xs
removeComments' (x:xs) = removeComments' xs
removeComments' [] = []


naiveTokenSingleLine :: String -> [Tokentype]
naiveTokenSingleLine "" = []
naiveTokenSingleLine (x:xs)
    | (x == '_') = if (length rest >= 1 && isValueToken (head rest)) then (combineToken x $ head rest):(tail rest) else (Value [x]):rest
    | isEmpty x = if (length rest >= 1 && (head rest) == Empty) then rest else Empty:rest
    | isSpecial x = (Special [x]):rest
    | otherwise =  if (length rest >= 1 && isValueToken (head rest)) then (combineToken x $ head rest):(tail rest) else (Value [x]):rest
    where 
        rest = naiveTokenSingleLine xs
        isSpecial = not . isAlphaNum
        isEmpty c = (c == '\t' || c == ' ')



multiSpecialList :: [String]
multiSpecialList = [":=", "<=", ">=", "&&", "||", "!=", "//", "%%", "%-", "-%", "[G=", "[T=", "..", ".lock", ".unlock"]

maxSpecialLength :: Int
maxSpecialLength = foldr max 0 $ map length multiSpecialList


-- combineSpecialToken :: [Tokentype] -> String
-- combineSpecialToken [] = ""
-- combineSpecialToken (Empty:_) = ""
-- combineSpecialToken ((Special "\n"):_) = "\n"
-- combineSpecialToken ((Special ";"):_) = ";"
-- combineSpecialToken ((Value x):xs) = x ++ combineSpecialToken xs
-- combineSpecialToken ((Special x):xs) = x ++ combineSpecialToken xs

combineSpecialToken :: [Tokentype] -> [String]
combineSpecialToken [] = [""]
combineSpecialToken (Empty:_) = [""]
combineSpecialToken ((Special "\n"):_) = ["\n"]
combineSpecialToken ((Special ";"):_) = [";"]
combineSpecialToken ((Value x):xs) = [x] ++ (combineSpecialToken xs)
combineSpecialToken ((Special x):xs) = [x] ++ (combineSpecialToken xs)




iterCombineSpecialToken :: [Tokentype] -> [Tokentype]
iterCombineSpecialToken [] = []
iterCombineSpecialToken (Empty:xs) = Empty:(iterCombineSpecialToken xs)
iterCombineSpecialToken (Value x:xs) = (Value x):(iterCombineSpecialToken xs)
iterCombineSpecialToken (Special x:xs) = if idx == -1 then (Special x):(iterCombineSpecialToken xs) else (Special (head $ drop idx matchRes)):(iterCombineSpecialToken (drop (idx-1) (xs)))
        where matchRes = take (maxSpecialLength+1) $ map (join "") $ inits $ combineSpecialToken (Special x:xs)
              indices = map (\x -> elem x multiSpecialList) matchRes
              maybeIdx = length $ fst $ splitAtFirst True indices
              idx = if (drop maybeIdx indices) == [] then -1 else maybeIdx

