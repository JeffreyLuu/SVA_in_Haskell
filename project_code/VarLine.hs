module VarLine where

import Variable
import Helper
import Token
import Keyword
import BExpr
import IExpr
import NumSet
import Overall
import ProgramStruct

import Control.Exception
import Data.Maybe
import Data.Either


------ For Variable: convert datatype ------------------------------------------------------------

isVarLine :: String -> Bool
isVarLine s = (xs /= []) && (elem (head xs) [Keyword Integer, Keyword Boolean, Keyword LockK, Keyword Dirty])
        where xs = tokenize s

getAllVariables :: [String] -> [Variable]
getAllVariables [] = []
getAllVariables (x:xs) = if isVarLine x then (getVarLineCmd token) ++ remain else remain
    where token = tokenize x
          remain = getAllVariables xs

----- printing of variable 
----- need to include the local variables from procs


getProcWLocalLocal :: ProcWLocal -> [Variable]
getProcWLocalLocal (CmdWLocal (_, _, _, xs, _)) = xs

getOverallVariables :: Overall -> [Variable]
getOverallVariables (Overall (globals, consts, procs, asserts, signals, progs)) = globals ++ locals
    where locals = foldr (++) [] $ map getProcWLocalLocal procs

printVarTypeSingleLine :: VarName -> [Variable] -> String
printVarTypeSingleLine name vars = if length vars == 0 then typeLine ++ varLine else varLine
    where typeLine = name ++ " :: {"++typeStr++"}\n"
          typeStr = if name!!1 == 'v' then "Var" else "Arr"
          varLine = name ++ " = {" ++ (join ", " $ map show vars) ++ "}"

-- print the datatype lines for Var/Arr

printVarDatatypeSingleLine :: VarName -> [Variable] -> String
printVarDatatypeSingleLine name vars = "datatype " ++ name ++ " = " ++ (if length str == 0 then dummy else str) 
    where str = join " | " $ map show vars
          dummy = "Dummy"++name

printVarDatatypeLine :: [Variable] -> String
printVarDatatypeLine vars = join "\n" [nomsLine, arrsLine]
    where noms = filter isNomVar vars
          arrs = filter isArrVar vars
          nomsLine = printVarDatatypeSingleLine "Var" noms
          arrsLine = printVarDatatypeSingleLine "Arr" arrs
          

printVarTypesLine :: [Variable] -> String
printVarTypesLine vars = join "\n" [ivline, ialine, bvline, baline, lvline, laline]
    where ivline = printVarTypeSingleLine "ivnums" $ filter isNomVar $ filter isIntVar vars
          ialine = printVarTypeSingleLine "ianums" $ filter isArrVar $ filter isIntVar vars
          bvline = printVarTypeSingleLine "bvnums" $ filter isNomVar $ filter isBoolVar vars
          baline = printVarTypeSingleLine "banums" $ filter isArrVar $ filter isBoolVar vars
          lvline = printVarTypeSingleLine "lvnums" $ filter isNomVar $ filter isLockVar vars
          laline = printVarTypeSingleLine "lanums" $ filter isArrVar $ filter isLockVar vars

printIdxTypeLineSingle :: ArrVar -> String
printIdxTypeLineSingle var = res
    where name = getArrVarName var
          size = fromJust $ getArrVarSize var
          res = "itype("++name++") = " ++ (show size)

printIdxTypeLine :: [Variable] -> String
printIdxTypeLine vars = join "\n" [(join "\n" $ map printIdxTypeLineSingle relevant), final]
    where relevant = filter (\x -> isJust $ getArrVarSize x) $ map getArrVar $ filter isArrVar vars
          final = "itype(_) = ditype"

printContentTpLineSingle :: ArrVar -> String
printContentTpLineSingle var = res
    where name = getArrVarName var
          size = fromJust $ getArrVarRange var
          res = "ctype("++name++") = " ++ (show size)

-- printContentTypeLine :: [Variable] -> String
-- printContentTypeLine vars = join "\n" [(join "\n" $ map printContentTpLineSingle relevant), final]
--     where relevant = filter (\x -> isJust $ getArrVarRange x) $ map getArrVar $ filter isArrVar $ filter isIntVar vars
--           final = "ctype(_) = dctype"

printContentTpSingle :: Variable -> String
printContentTpSingle var = join "" ["(", name, ",", range, ")"]
    where name = getVariableName var
          range = show $ fromJust $ getVariableRange var

printContentTypeLine :: [Variable] -> String
printContentTypeLine vars = join "" ["ctypeMap = (", nomStr, ",", arrStr, ")"]
    where relevant = filter (\x -> (getVariableRange x)/=Nothing) vars
          noms = filter (not . isArrVar) relevant
          arrs = filter isArrVar relevant
          nomStr = join "" ["<", join "," (map printContentTpSingle noms), ">"]
          arrStr = join "" ["<", join "," (map printContentTpSingle arrs), ">"]
          

          

getNomInitStr :: Variable -> String
getNomInitStr var = "("++name++","++(show val)++")"
    where name = getVariableName var
          val = getNomInitVal $ getNomVar var
          
getArrInitStr :: Variable -> String
getArrInitStr var = "("++name++","++(show val)++")"
    where name = getVariableName var
          val = getArrInitVal $ getArrVar var
          
printInitMapLine :: VarType -> String -> [Variable] -> String
printInitMapLine tp head vars = join "" [head, " = (", nomStr, ",", arrStr, ")"]
    where relevant = if tp == INT then filter isIntVar vars else filter (not . isIntVar) vars
          noms = filter (\x -> (getNomInitVal $ getNomVar x)/=UnknownV) $ filter (not. isArrVar) relevant
          arrs = filter (\x -> (getArrInitVal $ getArrVar x)/=UnknownA) $ filter isArrVar relevant
          nomStr = "<"++(join "," $ map getNomInitStr noms) ++ ">"
          arrStr = "<"++(join "," $ map getArrInitStr arrs) ++ ">"


printDirtyVarLine :: [Variable] -> String
printDirtyVarLine vars = "DirtyVars = Union({" ++ strs ++ "})"
    where dVars = filter isDirtyVar vars
          strs = join "," $ map getVarString dVars
          


printAllVarLines :: Overall -> String
printAllVarLines overall = join "\n\n" [datatypeLine, varTypeLine, dirtyVarLine, idxTpLine, (join "\n" [contentTpLine, initILine, initBLine])]
    where vars = getOverallVariables overall
          datatypeLine = printVarDatatypeLine vars
          varTypeLine = printVarTypesLine vars
          dirtyVarLine = printDirtyVarLine vars
          idxTpLine = printIdxTypeLine vars
          contentTpLine = printContentTypeLine vars
          initILine = printInitMapLine INT "initIMap" vars
          initBLine = printInitMapLine BOOL "initBMap" vars