module NumSet where

import Helper
import Token
import IExpr

import Control.Exception
import Data.Maybe

data NumSet = SSet (IExpr, IExpr) | LSet [IExpr] deriving (Eq)

instance Show NumSet where
    show (SSet (l, r)) = "{" ++ (show l) ++ ".." ++ (show r) ++ "}"
    show (LSet xs) = "{" ++ str ++ "}"
        where str = join "," $ map show xs

getDefaultSet :: Int -> NumSet
getDefaultSet i = SSet (Const 0, Const (i-1))


getNumSet :: [Tokentype] -> NumSet
getNumSet xs = assert check res
    where check = (head xs == (Special "{") && (last xs == (Special "}")))
          xs' = init $ tail xs
          isSSet = topLvlParenthesisElem (==(Special "..")) xs'
          (l, r) = topLvlParenthesisBreak (==(Special "..")) xs'
          left = getIExpr l
          right = getIExpr r
          sset = SSet (left, right)
          lset = LSet $ map getIExpr $ topLvlParenthesisSplit (==(Special ",")) xs'
          res = if isSSet then sset else lset

redNumSet :: [(VarName, Int)] -> NumSet -> Maybe [Int]
redNumSet mapping (SSet (l, r)) = res
    where lexp = subsIExpr mapping l
          rexp = subsIExpr mapping r
          success = (isConstIExpr lexp) && (isConstIExpr rexp)
          res = if success then Just [(getConstVal lexp)..(getConstVal rexp)] else Nothing
redNumSet mapping (LSet xs) = res
    where xss = map (subsIExpr mapping) xs
          success = all isConstIExpr xss
          res = if success then Just (map getConstVal xss) else Nothing
