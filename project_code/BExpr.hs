module BExpr where

import Token
import Helper
import IExpr
import Keyword

import Control.Exception
import Data.Maybe


-- data type for boolean expression

data BinBOps = And | Or | Xor deriving (Eq)

instance Show BinBOps where
    show And = "and"
    show Or = "or"
    show Xor = "xor" -- not defined yet

data CompOps = Eq | Neq | Gt | Ge | Lt | Le deriving (Eq)

instance Show CompOps where
    show Eq = "=="
    show Neq = "!="
    show Gt = ">"
    show Ge = ">="
    show Lt = "<"
    show Le = "<="

data BExpr = BVar VarName | BArc (VarName, IExpr) | 
              BTrue | BFalse | Not BExpr | BPARA (BExpr) |
              BBOp (BinBOps, BExpr, BExpr) | 
              CompOp (CompOps, IExpr, IExpr) | ErrorB
              deriving (Eq)

instance Show BExpr where
    show (BVar name) = name
    show (BArc (name, iexpr)) = name ++ "." ++ (show iexpr)
    show BTrue = "true"
    show BFalse = "false"
    show (Not expr) = "not(" ++ (show expr) ++ ")"
    show (BPARA expr) = "(" ++ (show expr) ++ ")"
    show (BBOp (op, l, r)) = join "" ["(", show l, ") ", show op, " (",  show r, ")"]
    show (CompOp (op, l, r)) = "(" ++ (show l) ++ ") " ++  (show op) ++ " (" ++ (show r) ++ ")"
    show (ErrorB) = "error"

showConstBExpr :: BExpr -> String
showConstBExpr BTrue = "true"
showConstBExpr BFalse = "false"
showConstBExpr x = show x

-- functions


getBinBOps :: Tokentype -> BinBOps
getBinBOps (Special "&&") = And
getBinBOps (Special "||") = Or
getBinBOps (Special "^") = Xor

getCompOps :: Tokentype -> CompOps
getCompOps (Special "=") = Eq
getCompOps (Special "!=") = Neq
getCompOps (Special ">") = Gt
getCompOps (Special ">=") = Ge
getCompOps (Special "<") = Lt
getCompOps (Special "<=") = Le
getCompOps x = error (show x)

checkBError :: [BExpr] -> Bool
checkBError xs = length errs > 0 
    where errs = filter (==ErrorB) xs

getBExpr :: [Tokentype] -> BExpr
getBExpr xs | (topLvlParenthesisElem bk xs) = res
    where bk x = elem x [Special "&&", Special "||", Special "^"]
          (l, r) = topLvlParenthesisBreak bk xs
          lexp = getBExpr l
          rexp = getBExpr r
          opToken = head $ drop (length l) xs
          res = if checkBError [lexp, rexp] then ErrorB else BBOp (getBinBOps opToken, lexp, rexp)
getBExpr xs | (topLvlParenthesisElem bk xs) = res
    where ls = [Special "=", Special "!=", Special ">", Special ">=", Special "<", Special "<="]
          bk x = elem x ls
          (l, r) = topLvlParenthesisBreak bk xs
          lexp = getIExpr l
          rexp = getIExpr r
          opToken = head $ drop (length l) xs
          res = if checkIError [lexp, rexp] then ErrorB else CompOp (getCompOps opToken, lexp, rexp)
getBExpr ((Special "("):xs) = assert (last xs == (Special ")")) res
    where exp' = getBExpr (init xs)
          res = if checkBError [exp'] then ErrorB else BPARA exp'
getBExpr xs | (elem (Special "[") xs) = if (last xs == (Special "]")) then res else ErrorB
    where (l, r) = break ((Special "[")==) (init xs)
          exp' = getIExpr $ tail r
          res = if checkIError [exp'] then ErrorB else BArc (getSingleVarName l, exp')
getBExpr ((Special "!"):xs) = Not (getBExpr xs)
getBExpr ((Keyword TrueK):[]) = BTrue
getBExpr ((Keyword FalseK):[]) = BFalse
getBExpr (x:[]) = BVar $ getTokenValue x
getBExpr _ = ErrorB

-- value substitution and calculation

bbopRed :: BinBOps -> BExpr -> BExpr -> BExpr
bbopRed And x y = bBoolMapping ((bBoolInverseMapping x) && (bBoolInverseMapping y))
bbopRed Or x y = bBoolMapping ((bBoolInverseMapping x) || (bBoolInverseMapping y))
bbopRed Xor x y = bBoolMapping (op (bBoolInverseMapping x) (bBoolInverseMapping y))
    where op x y = if x then not y else y

bBoolMapping :: Bool -> BExpr
bBoolMapping True = BTrue
bBoolMapping False = BFalse

bBoolInverseMapping :: BExpr -> Bool
bBoolInverseMapping BTrue = True
bBoolInverseMapping BFalse = False

isConstBExpr :: BExpr -> Bool
isConstBExpr x | elem x [BTrue, BFalse] = True
               | otherwise = False

bCompRed :: CompOps -> IExpr -> IExpr -> BExpr
bCompRed Eq x y = bBoolMapping $ (==) (getConstVal x) (getConstVal y)
bCompRed Neq x y = bBoolMapping $ (/=) (getConstVal x) (getConstVal y)
bCompRed Gt x y = bBoolMapping $ (>) (getConstVal x) (getConstVal y)
bCompRed Ge x y = bBoolMapping $ (>=) (getConstVal x) (getConstVal y)
bCompRed Lt x y = bBoolMapping $ (<) (getConstVal x) (getConstVal y)
bCompRed Le x y = bBoolMapping $ (<=) (getConstVal x) (getConstVal y)


isBVar :: BExpr -> Bool
isBVar (BVar _) = True
isBVar _ = False

needParamBool :: BExpr -> Bool
needParamBool (BBOp _) = True
needParamBool (CompOp _) = True
needParamBool (Not _) = True
needParamBool _ = False

getBExprName :: BExpr -> VarName
getBExprName (BVar name) = name
getBExprName (BArc (name, _)) = name
getBExprName _ = error "Invalid input"

getBExprArcIdx :: BExpr -> IExpr
getBExprArcIdx (BArc (_, idx)) = idx
getBExprArcIdx _ = error "Invalid input"


singleSubB :: [(VarName, BExpr)] -> VarName -> Maybe BExpr
singleSubB mapping name = res
    where match = filter (\x -> (fst x) == name) mapping
          res = if length match == 0 then Nothing else Just (snd $ head match)

subsBExpr :: [(VarName, BExpr)] -> [(VarName, Int)] -> BExpr -> BExpr
subsBExpr mapB mapI (BVar name) = res
    where match = singleSubB mapB name
          res = if isJust match then fromJust match else BVar name
subsBExpr mapB mapI (BArc (name, iexp)) = BArc (name, l')
    where l' = subsIExpr mapI iexp
subsBExpr mapB mapI BTrue = BTrue
subsBExpr mapB mapI BFalse = BFalse
subsBExpr mapB mapI (Not x) = Not (subsBExpr mapB mapI x)
subsBExpr mapB mapI (BPARA x) = BPARA (subsBExpr mapB mapI x)
subsBExpr mapB mapI (BBOp (op, l, r)) = res
    where l' = subsBExpr mapB mapI l
          r' = subsBExpr mapB mapI r
          canSub = (isConstBExpr l') && (isConstBExpr r')
          res  = if canSub then bbopRed op l' r' else BBOp (op, l', r')
subsBExpr mapB mapI (CompOp (op, l, r)) = res
    where l' = subsIExpr mapI l
          r' = subsIExpr mapI r
          canSub = (isConstIExpr l') && (isConstIExpr r')
          res = if canSub then bCompRed op l' r' else CompOp (op, l', r')
subsBExpr mapB mapI ErrorB = ErrorB

getBExprVarName :: BExpr -> VarName
getBExprVarName (BVar name) = name
getBExprVarName (BArc (name, exp)) = name
getBExprVarName _ = error "unexpected expression"
