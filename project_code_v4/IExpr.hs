module IExpr where

import Token
import Keyword
import Helper

import Control.Exception
import Data.Maybe
import Data.Char
import Text.Read



-- data type for integer expression

data BinIOps = Plus | Times | Minus | Div | Mod | MaxOp | MinOp deriving (Eq)

instance Show BinIOps where
    show Plus = "+"
    show Minus = "-"
    show Times = "*"
    show Div = "/"
    show Mod = "%"
    show MaxOp = "max"
    show MinOp = "min"

data UIOps = Uminus deriving (Eq)

instance Show UIOps where
    show Uminus = "-"

data IExpr = IVar VarName | IArc (VarName, IExpr) |
              Const Int | IPARA (IExpr) |
              BIOp (BinIOps, IExpr, IExpr) |
              UIOps (UIOps, IExpr) | ErrorI
              deriving (Eq)

instance Show IExpr where 
      show (IVar name) = name
      show (IArc (name, iexpr)) = name ++ "." ++ (show iexpr)
      show (Const c) = show c
      show (IPARA iexpr) = "(" ++ (show iexpr) ++ ")"
      show (BIOp (op, l, r)) = showBIOp op l r
      show (UIOps (op, a)) = (show op) ++ (show a)
      show (ErrorI) = "error"

-- functions

showBIOp :: BinIOps -> IExpr -> IExpr -> String
showBIOp op l r | elem op [MaxOp, MinOp] = join "" [show op, "(", show l, ", ", show r, ")"]
                | otherwise = join "" [show l, show op, show r]

getBinIOps :: Tokentype -> BinIOps
getBinIOps (Special "+") = Plus
getBinIOps (Special "-") = Minus
getBinIOps (Special "*") = Times
getBinIOps (Special "/") = Div
getBinIOps (Special "%") = Mod
getBinIOps (Keyword Max) = MaxOp
getBinIOps (Keyword Min) = MinOp

convertUminus :: [Tokentype] -> [Tokentype]
convertUminus xs = convertUminus' xs indices
          where lenCnt = length xs
                indices = map (\x -> lenCnt - x) $ getUminusIdx xs []
convertUminus' xs [] = xs
convertUminus' xs (y:ys) = convertUminus' ((take y xs) ++ [Keyword MinusSign] ++ (drop (y+1) xs)) ys

getUminusIdx :: [Tokentype] -> [Int] -> [Int]
getUminusIdx xs cache | r == [] = cache
                      | l == [] = getUminusIdx (tail r) ((length r):cache)
                      | (elem (last l) [(Special "("), Special "+", Special "-", Special "*", Special "/", Special "%"]) = getUminusIdx (tail r) ((length r):cache)
                      | otherwise = getUminusIdx (tail r) cache
              where (l, r) = break ((Special "-")==) xs 

checkIError :: [IExpr] -> Bool
checkIError xs = length errs > 0 
    where errs = filter (==ErrorI) xs


-- currently, if it sees a variable name, it just change it into a IVar, but this may be a constant
getIExpr' :: [Tokentype] -> IExpr
getIExpr' ((Keyword Max):xs) = assert ((head xs == (Special "(")) && (last xs == (Special ")"))) res
    where (l, r) = topLvlParenthesisBreak (== (Special ",")) (init $ tail xs) 
          lexp = getIExpr' l
          rexp = getIExpr' r
          res = if checkIError [lexp, rexp] then ErrorI else BIOp (MaxOp, lexp, rexp)
getIExpr' ((Keyword Min):xs) = assert ((head xs == (Special "(")) && (last xs == (Special ")"))) res
    where (l, r) = topLvlParenthesisBreak (== (Special ",")) (init $ tail xs) 
          lexp = getIExpr' l
          rexp = getIExpr' r
          res = if checkIError [lexp, rexp] then ErrorI else BIOp (MinOp, lexp, rexp)
getIExpr' xs  | (topLvlParenthesisElem bk xs) = res
    where bk x = elem x [Special "+", Special "-"]
          (l, r) = topLvlParenthesisBreak bk xs
          opToken = head $ drop (length l) xs
          lexp = getIExpr' l
          rexp = getIExpr' r
          res = if checkIError [lexp, rexp] then ErrorI else BIOp (getBinIOps opToken, lexp, rexp)
getIExpr' xs  | (topLvlParenthesisElem bk xs) = res
    where bk x = elem x [Special "*", Special "/", Special "%"]
          (l, r) = topLvlParenthesisBreak bk xs
          opToken = head $ drop (length l) xs
          lexp = getIExpr' l
          rexp = getIExpr' r
          res = if checkIError [lexp, rexp] then ErrorI else BIOp (getBinIOps opToken, lexp, rexp)
getIExpr' ((Keyword MinusSign):xs) = if checkIError [exp'] then ErrorI else UIOps(Uminus, exp')
    where exp' = getIExpr' xs
getIExpr' ((Special "("):xs) = assert (last xs == (Special ")")) res
    where exp' = getIExpr' $ init xs
          res = if checkIError [exp'] then ErrorI else IPARA exp'
getIExpr' xs | (elem (Special "[") xs) = if ((last xs == (Special "]"))) then res else ErrorI
    where (l, r) = break ((Special "[")==) (init xs)
          exp' = getIExpr' $ tail r
          res = if checkIError [exp'] then ErrorI else IArc (getSingleVarName l, exp')
getIExpr' (x:[]) = res
          where str = getTokenValue x
                res = if isValueToken x then (if (all isDigit str) then (Const $ strToInt str) else (IVar str)) else ErrorI
getIExpr' _ = ErrorI

getIExpr :: [Tokentype] -> IExpr
getIExpr xs = getIExpr' $ convertUminus xs


isConstIExpr :: IExpr -> Bool
isConstIExpr (Const _) = True
isConstIExpr _ = False

getConstVal :: IExpr -> Int
getConstVal (Const x) = x
getConstVal _ = error "Constant expression expected"

biopRed :: BinIOps -> Int -> Int -> Int
biopRed Plus x y = x+y
biopRed Minus x y = x-y
biopRed Times x y = x*y
biopRed Div x y = if y==0 then error "cannot be divided by 0" else div x y
biopRed Mod x y = if y==0 then error "cannot calculate mod by 0" else mod x y
biopRed MaxOp x y = if x>=y then x else y
biopRed MinOp x y = if x<=y then x else y


needParamInt :: IExpr -> Bool
needParamInt (BIOp _) = True
needParamInt (UIOps _) = True
needParamInt _ = False

getIExprName :: IExpr -> VarName
getIExprName (IVar name) = name
getIExprName (IArc (name, _)) = name
getIExprName _ = error "Invalid input"

isIVar :: IExpr -> Bool
isIVar (IVar _) = True
isIVar _ = False


getIExprArcIdx :: IExpr -> IExpr
getIExprArcIdx (IArc (_, idx)) = idx
getIExprArcIdx _ = error "Invalid input"

---- compute and substitute constants to get signalset
---- only substitute the ones in the arguments => 
---- the constants can be left unchanged 

singleSubI :: [(VarName, Int)] -> VarName -> Maybe Int
singleSubI mapping name = res
    where match = filter (\x -> (fst x) == name) mapping
          res = if length match == 0 then Nothing else Just (snd $ head match)

subsIExpr :: [(VarName, Int)] -> IExpr -> IExpr
subsIExpr mapping (Const x) = Const x
subsIExpr mapping (IPARA x) = subsIExpr mapping x
subsIExpr mapping (BIOp (op, l, r)) = res
    where l' = subsIExpr mapping l
          r' = subsIExpr mapping r
          canSub = (isConstIExpr l') && (isConstIExpr r')
          res = if canSub then Const $ biopRed op (getConstVal l') (getConstVal r') else BIOp (op, l', r')
subsIExpr mapping (UIOps (op, l)) = res
    where l' = subsIExpr mapping l
          canSub = isConstIExpr l'
          res = if canSub then Const (0-(getConstVal l')) else UIOps (op, l')
subsIExpr mapping (IVar name) = res
    where match = singleSubI mapping name
          res = if isJust match then Const $ fromJust match else IVar name
subsIExpr mapping (IArc (name, l)) = IArc (name, l')
    where l' = subsIExpr mapping l
subsIExpr mapping ErrorI = ErrorI

getIExprVarName :: IExpr -> VarName
getIExprVarName (IVar name) = name
getIExprVarName (IArc (name, exp)) = name
getIExprVarName _ = error "unexpected expression"
