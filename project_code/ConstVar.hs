module ConstVar where


import Helper
import AstProc
import Token
import Variable
import Keyword
import IExpr
import BExpr
import CSPLine
import NumSet

import Control.Exception
import Data.Maybe


reservedConstNames :: [VarName]
reservedConstNames = ["MinI", "MaxI", "ditype", "dctype", "InitB", "InitI", "ext_atomic"]

reservedWithSetVal = ["ditype", "dctype"]

-- parsing constant content
-- try to parse using getIExpr/getBExpr, if any successes, then use that
-- if none success, check if they are in the reservedWithSetVal, if so use NumSet
-- if still not successful, just keep it as a string


getConstVar :: String -> [Tokentype] -> Variable
getConstVar s xs  | (last xs == (Special ";")) = getConstVar (init s) $ init xs
                | otherwise = CstVar res
                    where (l, r) = topLvlParenthesisBreak (==(Special "=")) xs
                          canConvert = length l == 1
                          name = if canConvert then getTopLvlVarName l else concateStr l
                          iexpr = getIExpr r
                          bexpr = getBExpr r
                          numSet = getNumSet r
                          other = tail $ snd $ break (=='=') s
                          other' = if name == "LockOrder" then parseLockOrder r else other
                          shouldBeSet = elem name reservedWithSetVal
                          res = if (iexpr /= ErrorI) then ConstIV (name, iexpr) else if (bexpr /= ErrorB) then ConstBV (name, bexpr) else if shouldBeSet then ConstNumSet (name, numSet) else ConstOther (name, other')

showLockVar :: (VarName, Maybe IExpr) -> String
showLockVar (name, exp) = if isSingle then singleStr else arrStr
    where isSingle = exp == Nothing
          singleStr = "LV." ++ name
          arrStr = "LA." ++ name ++ "." ++ (show $ fromJust exp)

getLockVar :: [Tokentype] -> (VarName, Maybe IExpr)
getLockVar xs = assert checkCon (if isSingle then (name, Nothing) else (name, Just idx'))
    where isSingle = length xs == 1
          name = getSingleVarName $ take 1 xs
          checkCon = isSingle || ((xs!!1 == (Special "[")) && (last xs == (Special "]")))
          idx = getIExpr $ drop 2 $ init xs
          idx' = if idx == ErrorI then error (show xs) else idx

parseLockOrder :: [Tokentype] -> String
parseLockOrder xs = join "" ["<", lockStr, ">"]
    where checkCon = (head xs == (Special "<")) && (last xs == (Special ">"))
          content = init $ tail xs
          locks = map getLockVar $ topLvlParenthesisSplit (== (Special ",")) content
          lockStr = join "," $ map showLockVar locks
          

isConstLine :: String -> Bool
isConstLine s = (xs /= []) && (head xs == Keyword ConstK)
        where xs = tokenize s

getRawGlobalConstLine :: String -> [Variable]
getRawGlobalConstLine s | isConstLine s = [getConstVar s $ tail $ tokenize s]
                        | isCSPLine s = let s' = getCSPLine s in [getConstVar s' $ tokenize s']
                        | otherwise = []


getRawGlobalConstants :: [String] -> [Variable]
getRawGlobalConstants xs = foldr (++) [] $ map getRawGlobalConstLine xs


rawRedConst :: [(VarName, Int)] -> ConstVar -> (Bool, ConstVar)
rawRedConst mapping (ConstIV (name, iexp)) = (isCst, ConstIV (name, exp'))
    where exp' = subsIExpr mapping iexp
          isCst = isConstIExpr exp'
rawRedConst mapping (ConstNumSet (name, numSet)) = if isJust exp' then (True, ConstNumSet (name, numSet')) else (False, ConstNumSet (name, numSet))
    where exp' = redNumSet mapping numSet
          numSet' = LSet $ map (\x -> Const x) $ fromJust exp'

redConstants' :: [(VarName, Int)] -> [ConstVar] -> [ConstVar]
redConstants' mapping curr = if length addIn == 0 then redCsts else redConstants' newMap' redCsts
    where cache = curr
          redCstPairs = map (rawRedConst mapping) curr 
          redCsts = map snd redCstPairs
          newMap = map (\x -> (getCstVarName x, getConstVal $ getCstIVVal x)) $ filter isConstIV $ map snd $ filter fst redCstPairs
          addIn = filter (\x -> not $ elem x mapping) newMap
          newMap' = mapping ++ addIn


redConstants :: [ConstVar] -> [ConstVar]
redConstants consts = (redConstants' [] other) ++ bools
    where bools = filter (\x -> (isConstBV x) || (isCstOther x)) consts
          other = filter (\x -> (isConstIV x) || (isCstNumSet x)) consts

redConstVars :: [Variable] -> [Variable]
redConstVars vars = other ++ redCsts
    where csts = map getCstVar $ filter isCstVar vars
          other = filter (not . isCstVar) vars
          redCsts = map CstVar $ redConstants csts