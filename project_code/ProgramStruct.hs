module ProgramStruct where

import Helper
import Variable
import IExpr
import BExpr
import Token
import Event
import AstProc

import Data.Either
import Data.Maybe
import Control.Exception 

data ProgStruc = PStruct (VarName, [VarName], VarName) deriving (Eq)

-- var, optional idx, optional thread (to indicate if local)
data VariableStruct = VStruct (Variable, Int, Thread) | AStruct (Variable, Thread) deriving (Eq) -- if Variable is of type IV/BV, the VarName is ignored, otherwise this is for the idx

-- the int here are for id
data ProgramTree = LocLeaf (Int, ProgStruc, [VariableStruct]) | ProgLeaf (Int, ProgramTree, [VariableStruct]) | ProgNode (Int, [ProgramTree]) deriving (Show, Eq)

data Program = Prog (VarName, VarName, ProgramTree) deriving (Show, Eq)

data ProcWLocal = CmdWLocal (VarName, [VarName], Cmd, [Variable], [VariableStruct]) deriving (Show, Eq)

getProcWLocalName :: ProcWLocal -> VarName
getProcWLocalName (CmdWLocal (name, _, _, _, _)) = name

getProgStrucName :: ProgStruc -> VarName
getProgStrucName (PStruct (name, _, _)) = name

findMatchProcWLocal :: [ProcWLocal] -> VarName -> Maybe ProcWLocal
findMatchProcWLocal xs name = if length match == 0 then Nothing else Just $ head match
    where match = filter (\x -> getProcWLocalName x == name) xs


instance Show ProgStruc where
    show (PStruct (name, ps, th)) = name ++ "(" ++ str ++ th ++ ")"
        where str = foldr (\x y -> x ++ ", " ++ y) "" ps

-- instance Show VariableStruct where
--     show (VStruct (var, i, th)) = chan ++ "(" ++ paramStr ++ ")"
--         where thVal = getThreadName th
--               isLoc = isJust thVal
--               name = getVariableName var
--               chan = getChanName $ getVariableType var
--               thStr = if isLoc then "YesTh."++ fromJust thVal else "NoTh"
--               initFn = if isIntVar var then "initI" else "initB"
--               prefix = (if isIntVar var then "I" else "B") ++ (if isArrVar var then "A" else "V") 
--               param = if isArrVar var then (join "." [prefix, name, show i]) else (join "." [prefix, name]) -- [name, show i] else [name]
--               params1 = [param, initFn++"("++param++")", thStr]
--               paramStr = join ", " params1
--     show (AStruct (var, th)) = "||| i : "++idxRange++" @ "++chanStr
--         where name = getVariableName var
--               idxRange = "itypes("++name++")"
--               thVal = getThreadName th
--               isLoc = isJust thVal
--               chan = getChanName $ getVariableType var
--               thStr = if isLoc then "YesTh."++ fromJust thVal else "NoTh"
--               initFn = if isIntVar var then "initI" else "initB"
--               params = if isArrVar var then [name, "i"] else [name]
--               params1 = params++[initFn++"("++name++")", thStr]
--               paramStr = join ", " params1
--               chanStr = chan ++ "(" ++ paramStr ++ ")"

instance Show VariableStruct where
    show (VStruct (var, i, th)) = param
        where name = getVariableName var
              prefix = (if isIntVar var then "I" else "B") ++ (if isArrVar var then "A" else "V") 
              param = if isArrVar var then (join "." [prefix, name, show i]) else (join "." [prefix, name]) -- [name, show i] else [name]
    show (AStruct (var, th)) = param
        where name = getVariableName var
              prefix = (if isIntVar var then "I" else "B") ++ "A"
              param = (join "." [prefix, name])

printVStructInTree :: VariableStruct -> String
printVStructInTree (VStruct (var, i, th)) = join "" ["VS.(", name, ",", thStr, ")"]
    where name = show (VStruct (var, i, th))
          thVal = getThreadName th
          isLoc = isJust thVal
          thStr = if isLoc then "YesTh."++ fromJust thVal else "NoTh"
printVStructInTree (AStruct (var, th)) = show (AStruct (var, th))

printListVStructInTree :: [VariableStruct] -> String
printListVStructInTree vars = join "" ["<", varStr, ">"]
    where varStr = join "," $ map printVStructInTree vars

------------------------------------------------------

isProgramLine :: String -> Bool
isProgramLine s = (topLvlParenthesisElem (==(Special "<")) (tokenize s))

getPrograms :: [ProcWLocal] -> [String] -> [Program]
getPrograms procs [] = []
getPrograms procs (x:xs) | isProgramLine x = (parseProgramToken procs (tokenize x)):remain
                         | otherwise = remain
        where remain = getPrograms procs xs

parseProgramToken :: [ProcWLocal] -> [Tokentype] -> Program
parseProgramToken procs xs = Prog (pname, compress, progTree)
    where (l, r) = topLvlParenthesisBreak (==(Special "=")) xs
          pname = getSingleVarName l
          (l', r') = break (==(Special "<")) r
          compress = getSingleVarName l'
          progTree = parseProgTreeToken procs r'

angleBracketBreak :: [Tokentype] -> ([Tokentype], [Tokentype])
angleBracketBreak = topLvlBreak (\x -> elem x [Special "<", Special "("]) (\x -> elem x [Special ">", Special ")"]) (==(Special ","))


angleBracketSplit :: [Tokentype] -> [[Tokentype]]
angleBracketSplit = topLvlSplit (\x -> elem x [Special "<", Special "("]) (\x -> elem x [Special ">", Special ")"]) (==(Special ","))


parseProgTreeToken :: [ProcWLocal] -> [Tokentype] -> ProgramTree
parseProgTreeToken procs ((Special "<"):xs) = (assert (last xs == (Special ">"))) ProgNode (0, (map (parseProgTreeToken procs) xss))
    where xs' = init xs
          xss = angleBracketSplit xs'
parseProgTreeToken procs xs = if elem varName names then LocLeaf (0, PStruct (varName, params, "i"), []) else error "Undefined processor"
    where   (l, r) = break ((Special "(")==) xs
            varName = getSingleVarName l
            names = map getProcWLocalName procs
            paramTokens = split (tail' $ init r) ((Special ",")==)
            params = map getSingleVarName paramTokens

------------------------------------------------------

getVarStructName :: VariableStruct -> VarName
getVarStructName (VStruct (var, _, _)) = getVariableName var
getVarStructName (AStruct (var, _)) = getVariableName var


getTreeId :: ProgramTree -> Int
getTreeId (LocLeaf (id, _, _)) = id
getTreeId (ProgLeaf (id, _, _)) = id
getTreeId (ProgNode (id, _)) = id

flattenProgTree :: ProgramTree -> [ProgStruc]
flattenProgTree (LocLeaf (_, p, _)) = [p]
flattenProgTree (ProgLeaf (_, p, _)) = flattenProgTree p
flattenProgTree (ProgNode (_, ps)) = foldr (++) [] $ map (flattenProgTree) ps

flattenProgram :: Program -> [ProgStruc]
flattenProgram (Prog (_, _, tree)) = flattenProgTree tree

getPStructTh :: ProgStruc -> VarName
getPStructTh (PStruct (_, _, th)) = th

getSubTreeThs :: ProgramTree -> [VarName]
getSubTreeThs (LocLeaf (id, prog, vars)) = [getPStructTh prog]
getSubTreeThs (ProgLeaf (id, prog, vars)) = getSubTreeThs prog
getSubTreeThs (ProgNode (id, progs)) = foldr (++) [] $ map getSubTreeThs progs

progTreeFoldr :: (ProgramTree -> String) -> ProgramTree -> String
progTreeFoldr func (LocLeaf (id, prog, vars)) = func (LocLeaf (id, prog, vars))
progTreeFoldr func (ProgLeaf (id, prog, vars)) = subTreeLine++(func (ProgLeaf (id, prog, vars)))
    where subTreeLine = progTreeFoldr func prog
progTreeFoldr func (ProgNode (id, progs)) = subTreeLine++(func (ProgNode (id, progs)))
    where subTreeLine = join "" $ map (progTreeFoldr func) progs


programChangeSubTree :: (ProgramTree -> ProgramTree) -> Program -> Program
programChangeSubTree func (Prog (name, method, tree)) = Prog (name, method, func tree)



progTreeDepth :: ProgramTree -> Int
progTreeDepth (LocLeaf _) = 1
progTreeDepth (ProgLeaf (_, prog, _)) = 1 + (progTreeDepth prog)
progTreeDepth (ProgNode (_, trees)) = 1 + (maximum $ map progTreeDepth trees)


varStructContain :: VariableStruct -> VariableStruct -> Bool
varStructContain (VStruct (n1, _, _)) (VStruct (n2, _, _)) = n1 == n2
varStructContain (VStruct (n1, _, _)) (AStruct (n2, _)) = n1 == n2
varStructContain (AStruct _) _ = False


varStructElem :: VariableStruct -> [VariableStruct] -> Bool
varStructElem (VStruct (n1, i1, th)) ((VStruct (n2, i2, _)):xs) = if (n1==n2 && i1==i2) then True else varStructElem (VStruct (n1, i1, th)) xs
varStructElem (AStruct (n1, th)) ((AStruct (n2, _)):xs) = if (n1==n2) then True else varStructElem (AStruct (n1, th)) xs
varStructElem (VStruct (n1, i1, _)) _ = False
varStructElem (AStruct (n1, _)) _ = False

isLocVStrcut :: VariableStruct -> Bool
isLocVStrcut (VStruct (var, i, th)) = isJust $ getThreadName th
isLocVStrcut (AStruct (var, th)) = isJust $ getThreadName th

getProgTreeVStructs :: ProgramTree -> [VariableStruct]
getProgTreeVStructs (LocLeaf (_, _, vars)) = vars
getProgTreeVStructs (ProgLeaf (_, prog, vars)) = vars ++ (getProgTreeVStructs prog)
getProgTreeVStructs (ProgNode (id, progs)) = foldr (++) [] $ map getProgTreeVStructs progs

progTreeThCnt :: ProgramTree -> Int
progTreeThCnt (LocLeaf (_)) = 1
progTreeThCnt (ProgLeaf (_, prog, _)) = progTreeThCnt prog
progTreeThCnt (ProgNode (id, progs)) = foldr (+) 0 $ map progTreeThCnt progs

programThCnt :: Program -> Int
programThCnt prog = progTreeThCnt tree
    where Prog (name, method, tree) = prog