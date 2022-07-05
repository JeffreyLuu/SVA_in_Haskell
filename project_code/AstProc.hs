module AstProc where

import IExpr
import BExpr
import Keyword
import Token
import Helper
import Variable
import Signal

import Control.Exception


-- data type

data Cmd =  SkipP | 
            Sq (Cmd, Cmd) | 
            SQ [Cmd] | 
            IterC Cmd | 
            WhileC (BExpr, Cmd) |
            CondOne (BExpr, Cmd) |
            CondTwo (BExpr, Cmd, Cmd) |
            Iassign (IExpr, IExpr) |
            Bassign (BExpr, BExpr) |
            SigC SignalCmd |
            ISigC (SignalCmd, IExpr) |
            LockC (VarName, IExpr, Bool) | 
            AtomicC Cmd |
            ErrorC
            deriving (Show, Eq)


data AstProc = ProcDef (VarName, [VarName], Cmd) deriving (Show, Eq)


-- functions 

getCmd :: [Variable] -> [Tokentype] -> Cmd
getCmd vars xs | (topLvlParenthesisElem bk xs) = Sq (getCmd vars l, getCmd vars r)
          where bk x = elem x [Special ";"]
                (l, r) =  topLvlParenthesisBreak (==(Special ";")) xs
getCmd vars ((Keyword While):xs) = WhileC (con, todo)
      where (l, r) = break ((Keyword Do) ==) xs
            con = getBExpr l
            todo = getCmd vars $ tail' r
getCmd vars ((Keyword If):xs) = if (r == []) then CondOne (getBExpr l', getCmd vars $ tail' r') else CondTwo (getBExpr l', getCmd vars $ tail' r', getCmd vars $ tail' r)
      where (l, r) = break ((Keyword Else) ==) xs
            (l', r') = break ((Keyword Then) ==) l
getCmd vars ((Keyword Iter):xs) = IterC (getCmd vars xs)
getCmd vars ((Keyword Atomic):xs) = AtomicC (getCmd vars xs)
getCmd vars ((Keyword Sig):xs) = assert ((head xs == (Special "(")) && ((last xs) == (Special ")"))) SigC (getSignalChannel $ init $ tail' xs)
getCmd vars ((Keyword Isig):xs) = assert ((head xs == (Special "(")) && ((last xs) == (Special ")")))  ISigC (getSignalChannel l, getIExpr r)
      where ys = init $ tail' xs
            (l, r) = topLvlParenthesisBreak (\x -> elem x [Special ","]) ys 
getCmd vars ((Keyword Integer):xs) = SkipP
getCmd vars ((Keyword Boolean):xs) = SkipP
getCmd vars ((Keyword LockK):xs) = error "Lock cannot be declared locally"
getCmd vars xs | (topLvlParenthesisElem (\x -> elem x [Special ".lock", Special ".unlock"]) xs) = res
      where isLock = elem (Special ".lock") xs
            isArr = elem (Special "[") xs
            name = getSingleVarName [head xs]
            idxTokens = if isArr then fst $ break (==(Special "]")) $ tail $ snd $ break (==(Special "[")) xs else []
            idx = if isArr then getIExpr idxTokens else ErrorI
            res = LockC (name, idx, isLock)
getCmd vars xs | (topLvlParenthesisElem (==(Special ":=")) xs) = if isInt then Iassign (getIExpr l, getIExpr r) else Bassign (getBExpr l, getBExpr r)
          where (l, r) = topLvlParenthesisBreak (==(Special ":=")) xs
                varName = getTopLvlVarName l
                matches = filter (\x -> (getVariableName x) == varName) vars
                match = if length matches > 0 then head matches else error "Undefined variable"
                isInt = isIntVar match
getCmd vars ((Special "{"):xs) = if (last xs /= (Special "}")) then error (show xs) else getCmd vars (init xs) -- assert (last xs == (Special "}"))
getCmd vars ((Special "("):xs) = assert (last xs == (Special ")")) getCmd vars (init xs)
getCmd vars [] = SkipP
getCmd vars (Keyword Skip:[]) = SkipP
getCmd vars x = error (show x)

removeDoubleAtomic :: Cmd -> Cmd
removeDoubleAtomic (AtomicC x) = AtomicC (removeDoubleAtomic' x)
removeDoubleAtomic (Sq (l, r)) = Sq (removeDoubleAtomic l, removeDoubleAtomic r)
removeDoubleAtomic (SQ ls) = SQ (map removeDoubleAtomic ls)
removeDoubleAtomic (IterC c) = IterC (removeDoubleAtomic c)
removeDoubleAtomic (WhileC (b, c)) = WhileC (b, removeDoubleAtomic c)
removeDoubleAtomic (CondOne (b, c)) = CondOne (b, removeDoubleAtomic c)
removeDoubleAtomic (CondTwo (b, c, d)) = CondTwo (b, removeDoubleAtomic c, removeDoubleAtomic d)
removeDoubleAtomic x = x

removeDoubleAtomic' (AtomicC x) = (removeDoubleAtomic' x)
removeDoubleAtomic' (Sq (l, r)) = Sq (removeDoubleAtomic' l, removeDoubleAtomic' r)
removeDoubleAtomic' (SQ ls) = SQ (map removeDoubleAtomic' ls)
removeDoubleAtomic' (IterC c) = IterC (removeDoubleAtomic' c)
removeDoubleAtomic' (WhileC (b, c)) = WhileC (b, removeDoubleAtomic' c)
removeDoubleAtomic' (CondOne (b, c)) = CondOne (b, removeDoubleAtomic' c)
removeDoubleAtomic' (CondTwo (b, c, d)) = CondTwo (b, removeDoubleAtomic' c, removeDoubleAtomic' d)
removeDoubleAtomic' x = x


foldCmd :: (BExpr -> BExpr) -> (IExpr -> IExpr) -> Cmd -> Cmd
foldCmd f1 f2 SkipP = SkipP
foldCmd f1 f2 (Sq (l, r)) = Sq (foldCmd f1 f2 l, foldCmd f1 f2 r)
foldCmd f1 f2 (SQ xs) = SQ (map (foldCmd f1 f2) xs)
foldCmd f1 f2 (IterC c) = IterC (foldCmd f1 f2 c)
foldCmd f1 f2 (WhileC (b, c)) = WhileC (f1 b, foldCmd f1 f2 c)
foldCmd f1 f2 (CondOne (b, c)) = CondOne (f1 b, foldCmd f1 f2 c)
foldCmd f1 f2 (CondTwo (b, c, d)) = CondTwo (f1 b, foldCmd f1 f2 c, foldCmd f1 f2 d)
foldCmd f1 f2 (Iassign (l, r)) = Iassign (f2 l, f2 r)
foldCmd f1 f2 (Bassign (l, r)) = Bassign (f1 l, f1 r)
foldCmd f1 f2 (SigC sig) = SigC sig
foldCmd f1 f2 (ISigC (sig, exp)) = ISigC (sig, f2 exp)
foldCmd f1 f2 (AtomicC c) = AtomicC (foldCmd f1 f2 c)
foldCmd f1 f2 ErrorC = ErrorC

checkCmdNeedAtomic :: Cmd -> Bool
checkCmdNeedAtomic (AtomicC x) = True
checkCmdNeedAtomic (Sq (l, r)) = (checkCmdNeedAtomic l) || (checkCmdNeedAtomic r)
checkCmdNeedAtomic (SQ ls) = foldl (||) False $ map checkCmdNeedAtomic ls
checkCmdNeedAtomic (IterC x) = checkCmdNeedAtomic x
checkCmdNeedAtomic (WhileC (_, c)) = checkCmdNeedAtomic c
checkCmdNeedAtomic (CondOne (_, c)) = checkCmdNeedAtomic c
checkCmdNeedAtomic (CondTwo (_, c, d)) = (checkCmdNeedAtomic c) || (checkCmdNeedAtomic d)
checkCmdNeedAtomic _ = False

-- the overall function, which does renaming local variables and returning the AstProc
-- should also include a function to parse the part for associated variables
getProc :: [Variable] -> [Variable] -> [Tokentype] -> AstProc
getProc globals consts xs = (assert (topLvlParenthesisElem (==(Special "="))xs)) res
        where renamedTokens = renameProcToken xs
              pname = getProcName renamedTokens
              params = getProcParams renamedTokens
              locals = getLocalVariable renamedTokens
              cmd = removeDoubleAtomic $ getCmd (globals++consts++locals) $ getCmdToken renamedTokens
              res = ProcDef (pname, params, cmd)


getProcName :: [Tokentype] -> VarName
getProcName xs = getSingleVarName l'
        where (l, r) = topLvlParenthesisBreak (==(Special "=")) xs 
              (l', r') = break ((Special "(")==) l

getProcParams :: [Tokentype] -> [VarName]
getProcParams xs = params
        where (l, r) = topLvlParenthesisBreak (==(Special "=")) xs 
              (l', r') = break ((Special "(")==) l
              paramTokens = split (tail' $ init r') ((Special ",")==)
              params = map getSingleVarName paramTokens

getCmdToken :: [Tokentype] -> [Tokentype]
getCmdToken xs = r
        where (l, r) = topLvlParenthesisBreak (==(Special "=")) xs 


renameLocalVarName :: VarName -> VarName -> VarName
renameLocalVarName pname local = pname ++ "_" ++ local

renameLocalVars :: VarName -> [Variable] -> [Variable]
renameLocalVars pname locals = map (renameVarByFunc func) locals
    where func = renameLocalVarName pname

-- this renames all tokens, include the local variables 
-- so should avoid running this twice
renameProcToken ::[Tokentype] -> [Tokentype] 
renameProcToken xs = map func xs
    where pname = getProcName xs
          locals = getLocalVariable xs
          localNames = map getVariableName $ locals
          func x = if isValueToken x then let xVal = getTokenValue x in (if elem xVal localNames then (Value $ renameLocalVarName pname xVal) else x) else x


