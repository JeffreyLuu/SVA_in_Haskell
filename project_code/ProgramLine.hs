module ProgramLine where

import Helper
import Token
import Program
import ProgramStruct
import Overall
import Variable
import Signal
import IExpr
import BExpr
import AstProc
import Event
import NumSet

import Control.Exception
import Data.Either
import Data.Maybe
import Text.Read


getSubsMapping :: [Variable] -> ProcWLocal -> ProgStruc -> ([(VarName, BExpr)], [(VarName, Int)])
getSubsMapping consts procWLoc prog = (mapB, mapI ++ pairI2)
    where CmdWLocal (pname, ps, _, _, _) = procWLoc
          PStruct (pname', ps', th) = prog
          psInt = map strToInt ps'
          mapI = zip ps psInt
          validB = filter (\x -> isConstBV $ getCstVar x) consts
          mapB = zip (map getVariableName validB) (map getCstBVVal $ map getCstVar validB)
          validI = filter (\x -> isConstIV $ getCstVar x) consts
          nameI = map getVariableName validI
          expI = map (subsIExpr []) $ map getCstIVVal $ map getCstVar validI
          pairI = zip nameI expI
          pairI1 = filter (\x -> isConstIExpr (snd x)) pairI
          pairI2 = map (\x -> (fst x, getConstVal $ snd x)) pairI1

-- returns thread and list of signals for that thread
getSigSet :: [Variable] -> [ProcWLocal] -> ProgStruc -> (VarName, [SignalCmd])
getSigSet consts procs prog = (th, getRelevantSig mapB mapI cmd)
    where match' = findMatchProcWLocal procs $ getProgStrucName prog
          match = if isJust match' then fromJust match' else error "No matching proc defined"
          (mapB, mapI) = getSubsMapping consts match prog
          CmdWLocal (pname, _, cmd, _, _) = match
          PStruct (pname', ps', th) = prog

printSigSetLine :: Int -> (VarName, [SignalCmd]) -> String
printSigSetLine pid (th, sigs) = lineHead ++ " = {|" ++ sigStrs ++ "|}"
    where lineHead = getSigSetStr pid th
          sigStrs = join "," $ map printSignal sigs


getAllSigSet :: [Variable] -> [ProcWLocal] -> Int -> [ProgStruc] -> String
getAllSigSet consts procs pid progs = (join "\n" strs)
    where strs = map (printSigSetLine pid) $ map (getSigSet consts procs) progs

-- return a list of structs that are relevant (only global variables)
getReadSet :: [Variable] -> [Variable] -> [ProcWLocal] -> ProgStruc -> ThVarPair
getReadSet globs consts procs prog = (th, resSet)
    where match' = findMatchProcWLocal procs $ getProgStrucName prog
          match = if isJust match' then fromJust match' else error "No matching proc defined"
          (mapB, mapI) = getSubsMapping consts match prog
          CmdWLocal (pname, _, cmd, _, _) = match
          PStruct (pname', ps', th) = prog
          globNames = map getVariableName globs
          varSet = removeDuplicate $ getReadVarStruct mapB mapI cmd
          resSet = filter (\x -> elem (getVarStructName x) globNames) varSet

getWriteSet :: [Variable] -> [Variable] -> [ProcWLocal] -> ProgStruc -> ThVarPair
getWriteSet globs consts procs prog = (th, resSet)
    where match' = findMatchProcWLocal procs $ getProgStrucName prog
          match = if isJust match' then fromJust match' else error "No matching proc defined"
          (mapB, mapI) = getSubsMapping consts match prog
          CmdWLocal (pname, _, cmd, _, _) = match
          PStruct (pname', ps', th) = prog
          globNames = map getVariableName globs
          varSet = removeDuplicate $ getWriteVarStruct mapB mapI cmd
          resSet = filter (\x -> elem (getVarStructName x) globNames) varSet


getAllReadSet :: [Variable] -> [Variable] -> [ProcWLocal] -> [ProgStruc] -> [ThVarPair]
getAllReadSet globs consts procs progs = res
    where res = map (getReadSet globs consts procs) progs

getAllWriteSet :: [Variable] -> [Variable] -> [ProcWLocal] -> [ProgStruc] -> [ThVarPair]
getAllWriteSet globs consts procs progs = res
    where res = map (getWriteSet globs consts procs) progs

procLocalCompilation :: ProcWLocal -> ProgStruc -> ProgramTree
procLocalCompilation procWLocal prog = finalLeaf
    where (CmdWLocal (name1, ps1, cmd, local, assigned)) = procWLocal
          (PStruct (name2, ps2, th)) = prog
          prog1 = (assert (name1==name2)) PStruct (name1, ps2, th)
          localVars = map (createVarStructFromVar th) local
          locLeaf = LocLeaf (-1, prog1, localVars)
          finalLeaf = if length assigned == 0 then locLeaf else ProgLeaf (-1, locLeaf, assigned)

-- for (LocLeaf (i, prog, vars)) case, this might ignore the idx pre-assigned 
treeLocalCompilation :: [ProcWLocal] -> ProgramTree -> ProgramTree
treeLocalCompilation procs (LocLeaf (i, prog, vars)) = finalTree
    where progName = getProgStrucName prog
          match = filter (\x -> (getProcWLocalName x) == progName) procs
          localTree = if length match == 0 then error "No matching proc defined" else procLocalCompilation (head match) prog
          finalTree = if length vars == 0 then localTree else ProgLeaf (i, localTree, vars)
treeLocalCompilation procs (ProgLeaf (i, tree, vars)) = ProgLeaf (i, treeLocalCompilation procs tree, vars)
treeLocalCompilation procs (ProgNode (i, trees)) = ProgNode (i, map (treeLocalCompilation procs) trees)


progLocalCompilation :: [ProcWLocal] -> Program -> Program
progLocalCompilation procs prog = programChangeSubTree (treeLocalCompilation procs) prog


printSingleProgram :: [Variable] -> [Variable] -> [ProcWLocal] -> Int -> Program -> [String]
printSingleProgram globals consts procs pid prog = [sigStr, readMapStr, writeMapStr, (printProgramLines pid prog5)]
    where prog1 = progLocalCompilation procs prog
          prog3 = assignThToProg prog1
          sigStr = getAllSigSet consts procs pid (flattenProgram prog3)
          readMap = getAllReadSet globals consts procs (flattenProgram prog3)
          writeMap = getAllWriteSet globals consts procs (flattenProgram prog3)
          readMap' = expandMap globals consts readMap
          writeMap' = expandMap globals consts writeMap
          readMapStr = printThVSMaps "ReadVars" pid readMap'
          writeMapStr = printThVSMaps "WriteVars" pid writeMap'
          prog4 = allocateVars globals consts (writeMap', readMap') prog3
          prog5 = assignIdToProg prog4

          
mostProcLine :: [Program] -> String
mostProcLine progs = "mostproc = " ++ (show cnt) ++ "\n"
    where cnt = foldr max (0) $ map programThCnt progs

printThVSMaps :: String -> Int -> [ThVarPair] -> String
printThVSMaps head pid pairs = join "\n" $ map (printThVarPair head pid) pairs

printOverallProgram :: Overall -> String
printOverallProgram (Overall (globals, consts, procs, asserts, signals, progs)) = join "\n\n" [cntStr, sigLines, readLines, writeLines, progLines]
    where pairs = zip [1..] progs
          func = \(x, y) -> printSingleProgram globals consts procs x y
          progStrs = map func pairs
          sigLines = join "\n" $ map head progStrs
          readLines = join "\n" $ map (\x -> x!!1) progStrs
          writeLines = join "\n" $ map (\x -> x!!2) progStrs
          progLines = join "\n\n" $ map (\x -> x!!3) progStrs
          cntStr = mostProcLine progs
    -- where str = getAllProgramLine example
----- finish printing ------------------------------------------------------------



printProgHead :: ProgramTree -> String
printProgHead (LocLeaf (id, _, _)) = "Prog("++(show id)++")"
printProgHead (ProgLeaf (id, _, _)) = "Prog("++(show id)++")"
printProgHead (ProgNode (id, _)) = "Prog("++(show id)++")"

printProgTree2 :: ProgramTree -> String
printProgTree2 (LocLeaf (id, prog, vars)) = head ++ " = compress((" ++ progStr ++ "[|" ++ synStr ++ "|]" ++ varsStr1 ++ ") \\ " ++ synStr ++ ")"
    where head = printProgHead (LocLeaf (id, prog, vars))
          progStr = show prog
          varsStr = foldr (\x y -> if y=="" then x else x ++ "|||"++y) "" $ map show vars
          varsStr1 = "(" ++ varsStr ++ ")"
          synStr = getSynEvtHead (LocLeaf (id, prog, vars))
printProgTree2 (ProgLeaf (id, prog, vars)) = if length vars == 0 then res2 else res1
    where head = printProgHead (ProgLeaf (id, prog, vars))
          progStr = printProgHead prog
          varsStr = foldr (\x y -> if y=="" then x else x ++ "|||"++y) "" $ map show vars
          varsStr1 = "(" ++ varsStr ++ ")"
          synStr = getSynEvtHead (ProgLeaf (id, prog, vars))
          res1 = head ++ " = compress((" ++ progStr ++ "[|" ++ synStr ++ "|]" ++ varsStr1 ++ ") \\ " ++ synStr ++ ")"
          res2 = head ++ " = " ++ progStr
printProgTree2 (ProgNode (id, progs)) = head++" = compress((|| i: {"++range++"} @ ["++allowStr++"] "++progStr++") \\ chans)"
    where head = printProgHead (ProgNode (id, progs))
          sub_ids = map show $ map getTreeId progs
          range = join "," sub_ids
          allowStr = "Allow(i)"
          progStr = "Prog(i)"

printTreeIdx :: [Int] -> String
printTreeIdx xs = join "" ["<", intStr, ">"]
    where intStr = join "," $ map show xs

printProgTree :: VarName -> ProgramTree -> [Int] -> String
printProgTree name (LocLeaf (id, prog, vars)) idx = currLine
    where idxStr = printTreeIdx idx
          procApply = show prog
          thStr = getPStructTh prog
          procStr = join "" ["ProgLeaf.(", procApply, ",", thStr, ")"]
          varStr = printListVStructInTree vars
          currLine = join "" [name, "_sub(", idxStr, ") = ProgNode.(", procStr, ",", varStr, ")"]
printProgTree name (ProgLeaf (id, prog, vars)) idx = join "\n" [currLine, subProgLine]
    where idxStr = printTreeIdx idx
          subProgIdx = idx ++ [1]
          subProgStr = join "" [name, "_sub(", printTreeIdx subProgIdx, ")"]
          varStr = printListVStructInTree vars
          currLine = join "" [name, "_sub(", idxStr, ") = ProgNode.(", subProgStr, ",", varStr, ")"]
          subProgLine = printProgTree name prog subProgIdx
printProgTree name (ProgNode (id, progs)) idx = join "\n" [currLine, subProgLines]
    where idxStr = printTreeIdx idx
          cnt = length progs
          indices = map (\x -> idx ++ [x]) [1..cnt]
          subProgStrs = join "," $ map (\x -> join "" [name, "_sub(", printTreeIdx x, ")"]) indices
          currLine = join "" [name, "_sub(", idxStr, ") = ProgList.(<", subProgStrs, ">)"]
          subProgLineFunc = \(xx,yy) -> printProgTree name xx yy
          subProgLines = join "\n" $ map subProgLineFunc $ zip progs indices

printProgram :: Int -> Program -> String
printProgram pid (Prog (name, comp, tree)) = line
    where line = join "" [name, " = (ProgramRun(", subProg, ", Thread, True, ", show pid, ") [|AllLockEvts|] Lock_Chans) \\ Union({AllAtomicSet, AllLockEvts, chans})"]
          subProg = join "" [name, "_sub(<>)"]

printProgramLines :: Int -> Program -> String
printProgramLines pid (Prog (name, comp, tree)) = join "\n\n" [subLine, finalLine]
    where subLine = printProgTree name tree []
          finalLine = printProgram pid (Prog (name, comp, tree))

printProgram2 :: Program -> String
printProgram2 (Prog (name, comp, tree)) = line
    where line = name ++ " = " ++ (printProgHead tree)

getVarEvtSet :: VariableStruct -> String
getVarEvtSet (VStruct (var, i, th)) = getVarEvtSet' var (show i) th
getVarEvtSet (AStruct (var, th)) = "Union({"++singleStr++"| i <- "++idxRange++"})"
    where singleStr = getVarEvtSet' var "i" th
          n = getVariableName var
          idxRange = "itypes("++n++")"

getVarEvtSet' :: Variable -> String -> Thread -> String
getVarEvtSet' var i th = setName ++ "(" ++ paramStr ++ ")"
    where isLoc = isJust $ getThreadName th
          isArr = isArrVar var
          n = getVariableName var
          setName = getVarEvtSetName isLoc isArr
          tpCh = if isIntVar var then "I" else "B"
          param = if isArr then (join "." [tpCh ++"A", n, i]) else (join "." [tpCh ++ "V", n])
          thVal = getThreadName th
          params1 = if isLoc then [param, fromJust thVal] else [param]
          paramStr = join ", " params1

getVarsEvtSet :: [VariableStruct] -> String
getVarsEvtSet vars = "Union({" ++ (join ", " strs) ++ "})"
    where strs = map getVarEvtSet vars

getProgStrucEvtSet :: ProgStruc -> String
getProgStrucEvtSet (PStruct (name, ps, th)) = "ThreadEvtSet("++th++")"


getVarEvtSetName :: Bool -> Bool -> String
getVarEvtSetName isLoc isArr = start
    where start = if isLoc then "LocV" else "GlbV"
        --   end = if isArr then "A" else "V"

-- getSubTreeThs
getSyncRWSet :: [ThVarPair] -> [VarName] -> [VariableStruct]
getSyncRWSet xss ths = filter (\x -> not $ elem x invalid_vars) $ filter (not . isLocVStrcut) valid_vars
    where valid_xss = filter (\x -> elem (fst x) ths) xss
          invalid_xss = filter (\x -> not $ elem (fst x) ths) xss
          valid_vars = foldr (\x y -> (snd x) ++ y) [] valid_xss
          invalid_vars = foldr (\x y -> (snd x) ++ y) [] invalid_xss

getVStructName :: VariableStruct -> String
getVStructName (VStruct (var, i, th)) = if isArr then join "." [prefix, name, show i] else join "." [prefix, name]
    where isArr = isArrVar var
          isInt = isIntVar var
          name = getVariableName var
          prefix = (if isInt then "I" else "B") ++ (if isArr then "A" else "V")


getRWSetStr :: Bool -> VariableStruct -> String
getRWSetStr isWrite (VStruct (var, i, th)) = join "" [setName, "(", name, ")"]
    where name = getVStructName (VStruct (var, i, th))
          setName = if isWrite then "writeSet" else "readSet"


-- *************************
getSynEvtSet :: ([ThVarPair], [ThVarPair]) -> ProgramTree -> String
getSynEvtSet (writeMap, readMap) (LocLeaf (id, prog, vars)) = (getSynEvtHead (LocLeaf (id, prog, vars)))++" = "++str
    where progSet = getProgStrucEvtSet prog
          varSet = getVarsEvtSet vars
          subtreeVar = getProgTreeVStructs (LocLeaf (id, prog, vars))
          uniqueWrite = filter (\x -> varStructElem x subtreeVar ) $ getSyncRWSet writeMap (getSubTreeThs (LocLeaf (id, prog, vars))) 
          uniqueRead = filter (\x -> varStructElem x subtreeVar ) $ getSyncRWSet readMap (getSubTreeThs (LocLeaf (id, prog, vars))) 
          interStr = "inter("++progSet++","++varSet++")"
          uniqueStr = (join ", " $ (map (getRWSetStr True) uniqueWrite) ++ (map (getRWSetStr False) uniqueRead))
          str = if uniqueStr == "" then interStr else join "" ["Union({", join ", " [interStr, uniqueStr], "})"]
getSynEvtSet (writeMap, readMap) (ProgLeaf (id, prog, vars)) = (getSynEvtHead (ProgLeaf (id, prog, vars)))++" = "++str
    where progSet = getAllowEvtHead prog
          varSet = getVarsEvtSet vars
          subtreeVar = getProgTreeVStructs (ProgLeaf (id, prog, vars))
          uniqueWrite = filter (\x -> varStructElem x subtreeVar ) $ getSyncRWSet writeMap (getSubTreeThs (ProgLeaf (id, prog, vars)))
          uniqueRead = filter (\x -> varStructElem x subtreeVar ) $ getSyncRWSet readMap (getSubTreeThs (ProgLeaf (id, prog, vars)))
          interStr = "inter("++progSet++","++varSet++")"
          uniqueStr = (join ", " $ (map (getRWSetStr True) uniqueWrite) ++ (map (getRWSetStr False) uniqueRead))
          str = if uniqueStr == "" then interStr else join "" ["Union({", join ", " [interStr, uniqueStr], "})"]
getSynEvtSet (writeMap, readMap) (ProgNode (id, progs)) = (getSynEvtHead (ProgNode (id, progs)))++" = "++"Union({"++progSets++"})" 
    where progSets = join ", " $ map getSynEvtHead progs -- this case should never be used
          

getSynEvtHead :: ProgramTree -> String
getSynEvtHead (LocLeaf (id, _, _)) = "SyncOn("++(show id)++")"
getSynEvtHead (ProgLeaf (id, _, _)) = "SyncOn("++(show id)++")"
getSynEvtHead (ProgNode (id, _)) = "SyncOn("++(show id)++")"

getAllowEvtHead :: ProgramTree -> String
getAllowEvtHead (LocLeaf (id, _, _)) = "Allow("++(show id)++")"
getAllowEvtHead (ProgLeaf (id, _, _)) = "Allow("++(show id)++")"
getAllowEvtHead (ProgNode (id, _)) = "Allow("++(show id)++")"

getSigSetStr :: Int -> String -> String
getSigSetStr pid i = join "" ["sigSet(", show pid, ",", i, ")"]--"sigSet("++i++")"

getErrSet :: String -> String
getErrSet i = "errSet("++i++")"



getTreeSigSet :: ProgramTree -> String
getTreeSigSet tree = "Union({"++(getSigSetStr 1 "i")++" | i <-{"++idsStr++"}})"
    where ids = getSubTreeThs tree
          idsStr = join "," ids

getTreeErrSet :: ProgramTree -> String
getTreeErrSet tree = "Union({"++getErrSet("i")++" | i <-{"++idsStr++"}})"
    where ids = getSubTreeThs tree
          idsStr = join "," ids

getAllowEvtSet :: ProgramTree -> String
getAllowEvtSet (LocLeaf (id, prog, vars)) = (getAllowEvtHead (LocLeaf (id, prog, vars)))++" = "++"Union({"++progSet++","++varSet++","++sigStr++","++errStr++"})"
    where progSet = getProgStrucEvtSet prog
          varSet = getVarsEvtSet vars
          sigStr = getTreeSigSet (LocLeaf (id, prog, vars))
          errStr = getTreeErrSet (LocLeaf (id, prog, vars))
getAllowEvtSet (ProgLeaf (id, prog, vars)) = (getAllowEvtHead (ProgLeaf (id, prog, vars)))++" = "++"Union({"++progSet++","++varSet++"})"
    where progSet = getAllowEvtHead prog
          varSet = getVarsEvtSet vars
          sigStr = getTreeSigSet (ProgLeaf (id, prog, vars))
          errStr = getTreeErrSet (ProgLeaf (id, prog, vars))
getAllowEvtSet (ProgNode (id, progs)) = (getAllowEvtHead (ProgNode (id, progs)))++" = "++"Union({"++progSets++"})"
    where progSets = join ", " $ map getAllowEvtHead progs

getAllSyncLine :: ([ThVarPair], [ThVarPair]) -> ProgramTree -> String
getAllSyncLine (writeMap, readMap) tree = progTreeFoldr func tree
    where func = \x -> (getSynEvtSet (writeMap, readMap) x) ++ "\n"

getAllAllowLine :: ProgramTree -> String
getAllAllowLine tree = progTreeFoldr func tree
    where func = \x -> (getAllowEvtSet x) ++ "\n"

getAllTreeDeclareLine :: ProgramTree -> String
getAllTreeDeclareLine tree = progTreeFoldr func tree
    where func = \x -> (printProgTree2 x) ++ "\n"

getAllProgramLine :: ([ThVarPair], [ThVarPair]) -> Program -> String
getAllProgramLine (writeMap, readMap) (Prog (name, comp, tree)) = join "\n\n" [syncLines, allowLines, treeLines, programLine]
    where syncLines = getAllSyncLine (writeMap, readMap) tree
          allowLines = getAllAllowLine tree
          treeLines = getAllTreeDeclareLine tree
          programLine = printProgram2 (Prog (name, comp, tree))

--- for variables
createVarStructFromVar :: VarName -> Variable -> VariableStruct
createVarStructFromVar th var = res
    where isVar = isNomVar var
          thStr = YesTh (Output, th)
          res = if isVar then VStruct (var, -1, thStr) else AStruct (var, thStr)
          
createVarStructFromGlobVar :: Variable -> VariableStruct
createVarStructFromGlobVar var = res
    where isVar = isNomVar var
          thStr = NoTh
          res = if isVar then VStruct (var, -1, thStr) else AStruct (var, thStr)

--- assign thread to program 

assignThToPStruct :: Int -> ProgStruc -> ProgStruc
assignThToPStruct n (PStruct (name, ps, _)) = PStruct (name, ps, show n)

assignThToLocVStruct :: Int -> VariableStruct -> VariableStruct
assignThToLocVStruct n (VStruct (var, i, _)) = VStruct (var, i, YesTh (Output, show n))
assignThToLocVStruct n (AStruct (var, _)) = AStruct (var, YesTh (Output, show n))


assignThToProgTree :: Int -> ProgramTree -> (Int, ProgramTree)
assignThToProgTree n (LocLeaf (i, struc, vars)) = (n+1, LocLeaf (i, struc', vars'))
        where struc' = assignThToPStruct n struc
              vars' = map (assignThToLocVStruct n) vars
assignThToProgTree n (ProgLeaf (i, prog, vars)) = (n', ProgLeaf (i, prog', vars))
        where (n', prog') = assignThToProgTree n prog
assignThToProgTree n (ProgNode (i, structs)) = (n', ProgNode (i, structs'))
        where func' = \ (k, x) y -> (k, y++[x])
            --   func = \ x (k, y) -> func' (assignThToProgTree k x) y
            --   (n', structs') = foldr func (n, []) structs
              func = \(k, y) x -> func' (assignThToProgTree k x) y
              (n', structs') = foldl func (n, []) structs

assignThToProg :: Program -> Program
assignThToProg prog = programChangeSubTree func prog
    where func x = snd $ assignThToProgTree 1 x

---- assign id of each node to the tree
assignIdToProgTree :: Int -> ProgramTree -> (Int, ProgramTree)
assignIdToProgTree n (LocLeaf (_, struc, vars)) = (n+1, LocLeaf (n, struc, vars))
assignIdToProgTree n (ProgLeaf (_, prog, vars)) = (n', ProgLeaf (n, prog', vars))
        where (n', prog') = assignIdToProgTree (n+1) prog
assignIdToProgTree n (ProgNode (_, structs)) = (n', ProgNode (n, structs'))
        where func' = \ (k, x) y -> (k, x:y)
              func = \ x (k, y) -> func' (assignIdToProgTree k x) y
              (n', structs') = foldr func (n+1, []) structs

assignIdToProg :: Program -> Program
assignIdToProg prog = programChangeSubTree func prog
    where func x = snd $ assignIdToProgTree 1 x

---- naive allocation of global variables to minimize depth of the tree
---- other allocation strategy as possible
minAssignVar :: (ProgramTree -> Int) -> VariableStruct -> ProgramTree -> ProgramTree
minAssignVar calc var (LocLeaf (i, struct, vars)) = res
    where res = ProgLeaf (-1, (LocLeaf (i, struct, vars)), [var])
minAssignVar calc var (ProgLeaf (i, tree, vars)) = res
    where res = ProgLeaf (i, minAssignVar calc var tree, vars)
minAssignVar calc var (ProgNode (i, trees)) = res
    where trees' = map (minAssignVar calc var) trees
          vals = map calc trees'
          mini = minimum vals
          idx = matchIdx vals [mini]
          resTrees = (take idx trees) ++ [trees'!!idx] ++ (drop (idx+1) trees)
          res = ProgNode (i, resTrees)

minAssignAllVar :: (ProgramTree -> Int) -> [VariableStruct] -> ProgramTree -> ProgramTree
minAssignAllVar calc vars tree = resTree
    where func = minAssignVar calc
          resTree = foldr func tree vars

minAssignAllVarProg :: (ProgramTree -> Int) -> [VariableStruct] -> Program -> Program
minAssignAllVarProg calc vars prog = programChangeSubTree func prog
    where func = minAssignAllVar calc vars 

--------------------------- syntatic analysis ---------------------------

getBExprReadVarStruct :: BExpr -> [VariableStruct]
getBExprReadVarStruct (BVar name) = [VStruct (var, -1, NoTh)]
    where var = createLocalNomVar BOOL name 
getBExprReadVarStruct (BArc (name, iexp)) = res
    where res = if isConstIExpr iexp then [VStruct (var, getConstVal iexp, NoTh)]++remain else [AStruct (var, NoTh)]++remain
          var = createLocalArrVar BOOL name 
          remain = getIExprReadVarStruct iexp
getBExprReadVarStruct (Not x) = getBExprReadVarStruct x
getBExprReadVarStruct (BPARA x) = getBExprReadVarStruct x
getBExprReadVarStruct (BBOp (_, l, r)) = (getBExprReadVarStruct l) ++ (getBExprReadVarStruct r)
getBExprReadVarStruct (CompOp (_, l, r)) = (getIExprReadVarStruct l) ++ (getIExprReadVarStruct r)
getBExprReadVarStruct _ = []

getIExprReadVarStruct :: IExpr -> [VariableStruct]
getIExprReadVarStruct (IVar name) = [VStruct (var, -1, NoTh)]
    where var = createLocalNomVar INT name 
getIExprReadVarStruct (IArc (name, iexp)) = res
    where res = if isConstIExpr iexp then [VStruct (var, getConstVal iexp, NoTh)]++remain else [AStruct (var, NoTh)]++remain
          var = createLocalArrVar INT name 
          remain = getIExprReadVarStruct iexp
getIExprReadVarStruct (IPARA x) = getIExprReadVarStruct x
getIExprReadVarStruct (BIOp (_, l, r)) = (getIExprReadVarStruct l) ++ (getIExprReadVarStruct r)
getIExprReadVarStruct (UIOps (_, l)) = getIExprReadVarStruct l
getIExprReadVarStruct _ = []


getReadVarStruct :: [(VarName, BExpr)] -> [(VarName, Int)] -> Cmd -> [VariableStruct]
getReadVarStruct mapB mapI SkipP = []
getReadVarStruct mapB mapI (Sq (l, r)) = (getReadVarStruct mapB mapI l) ++ (getReadVarStruct mapB mapI r)
getReadVarStruct mapB mapI (SQ xs) = foldr (++) [] $ map (getReadVarStruct mapB mapI) xs
getReadVarStruct mapB mapI (IterC x) = getReadVarStruct mapB mapI x
getReadVarStruct mapB mapI (WhileC (con, x)) = varCon ++ (getReadVarStruct mapB mapI x)
    where subCon = subsBExpr mapB mapI con
          varCon = getBExprReadVarStruct subCon
getReadVarStruct mapB mapI (CondOne (con, x)) = varCon ++ (getReadVarStruct mapB mapI x)
    where subCon = subsBExpr mapB mapI con
          varCon = getBExprReadVarStruct subCon
getReadVarStruct mapB mapI (CondTwo (con, l, r)) = varCon ++ (getReadVarStruct mapB mapI l) ++ (getReadVarStruct mapB mapI r)
    where subCon = subsBExpr mapB mapI con
          varCon = getBExprReadVarStruct subCon
getReadVarStruct mapB mapI (LockC (name, idx, bool)) = getIExprReadVarStruct $ subsIExpr mapI idx
getReadVarStruct mapB mapI (Iassign (l, r)) = (tail varL) ++ varR
    where varL = getIExprReadVarStruct $ subsIExpr mapI l
          varR = getIExprReadVarStruct $ subsIExpr mapI r
getReadVarStruct mapB mapI (Bassign (l, r)) = (tail varL) ++ varR
    where varL = getBExprReadVarStruct $ subsBExpr mapB mapI l
          varR = getBExprReadVarStruct $ subsBExpr mapB mapI r
getReadVarStruct mapB mapI (SigC (name, xs)) = vars
    where vars = foldr (++) [] $ map (getIExprReadVarStruct . (subsIExpr mapI)) xs
getReadVarStruct mapB mapI (ISigC ((name, xs), exp)) = vars
    where vars = foldr (++) [] $ map (getIExprReadVarStruct . (subsIExpr mapI)) (exp:xs)
getReadVarStruct mapB mapI (AtomicC x) = getReadVarStruct mapB mapI x
getReadVarStruct mapB mapI ErrorC = []


getWriteVarStruct :: [(VarName, BExpr)] -> [(VarName, Int)] -> Cmd -> [VariableStruct]
getWriteVarStruct mapB mapI SkipP = []
getWriteVarStruct mapB mapI (Sq (l, r)) = (getWriteVarStruct mapB mapI l) ++ (getWriteVarStruct mapB mapI r)
getWriteVarStruct mapB mapI (SQ xs) = foldr (++) [] $ map (getWriteVarStruct mapB mapI) xs
getWriteVarStruct mapB mapI (IterC x) = getWriteVarStruct mapB mapI x
getWriteVarStruct mapB mapI (WhileC (con, x)) = getWriteVarStruct mapB mapI x
getWriteVarStruct mapB mapI (CondOne (con, x)) = getWriteVarStruct mapB mapI x
getWriteVarStruct mapB mapI (CondTwo (con, l, r)) = (getWriteVarStruct mapB mapI l) ++ (getWriteVarStruct mapB mapI r)
getWriteVarStruct mapB mapI (Iassign (l, r)) = [head varL]
    where varL = getIExprReadVarStruct $ subsIExpr mapI l
getWriteVarStruct mapB mapI (Bassign (l, r)) = [head varL]
    where varL = getBExprReadVarStruct $ subsBExpr mapB mapI l
getWriteVarStruct mapB mapI (AtomicC x) = getWriteVarStruct mapB mapI x
getWriteVarStruct mapB mapI _ = []

getRelevantSig :: [(VarName, BExpr)] -> [(VarName, Int)] -> Cmd -> [SignalCmd]
getRelevantSig mapB mapI (Sq (l, r)) = (getRelevantSig mapB mapI l) ++ (getRelevantSig mapB mapI r)
getRelevantSig mapB mapI (SQ xs) = foldr (++) [] $ map (getRelevantSig mapB mapI) xs
getRelevantSig mapB mapI (IterC x) = getRelevantSig mapB mapI x
getRelevantSig mapB mapI (WhileC (con, x)) = getRelevantSig mapB mapI x
getRelevantSig mapB mapI (CondOne (con, x)) = getRelevantSig mapB mapI x
getRelevantSig mapB mapI (CondTwo (con, l, r)) = (getRelevantSig mapB mapI l) ++ (getRelevantSig mapB mapI r)
getRelevantSig mapB mapI (AtomicC x) = getRelevantSig mapB mapI x
getRelevantSig mapB mapI (SigC (name, xs)) = [res]
    where sig' = (name, map (subsIExpr mapI) xs)
          res = keepPrefSig sig'
getRelevantSig mapB mapI (ISigC ((name, xs), _)) = [res]
    where sig' = (name, map (subsIExpr mapI) xs)
          res = keepPrefSig sig'
getRelevantSig mapB mapI _ = []


findConstIVal :: VarName -> [Variable] -> Int
findConstIVal name consts = res
    where match = filter (\y -> getVariableName y == name) consts
          matchVar = assert (length match > 0) head match
          cstVar = getCstVar matchVar
          iexpr = if isConstIV cstVar then getCstIVVal cstVar else error "Not InitIV, which is expected"
          iexpr' = subsIExpr [] iexpr
          res = if isConstIExpr iexpr' then getConstVal iexpr' else error "The expression cannot be reduced to a constant integer"

reduceConstIVal :: VarName -> [Variable] -> Int
reduceConstIVal name consts = if isJust res then (fromJust res) else findConstIVal name consts
    where res = readMaybe name :: Maybe Int
          

expandMap :: [Variable] -> [Variable] -> [ThVarPair] -> [ThVarPair]
expandMap globs consts [] = []
expandMap globs consts ((name, vs):xs) = (name, this):remain
    where remain = expandMap globs consts xs
          this = foldr (++) [] $ map (expandAStruct globs consts) vs

calcUniqueVars :: [Variable] -> [Variable] -> ([ThVarPair], [ThVarPair]) -> ([ThVarPair], [ThVarPair])
calcUniqueVars globs consts (wMap, rMap) = (wUnique, rUnique')
    where rMap' = expandMap globs consts rMap
          wMap' = expandMap globs consts wMap
          rUnique = findUniqueAccess rMap'
          wUnique = findUniqueAccess wMap'
          allWUnique = joinVarStructMap wUnique
          func = \(th, xs) -> (th, filter (\y -> not $ varStructElem y allWUnique) xs)
          rUnique' = map func rUnique


expandAStruct :: [Variable] -> [Variable] -> VariableStruct -> [VariableStruct]
expandAStruct globs consts (VStruct x) = [VStruct x]
expandAStruct globs consts (AStruct (x, th)) = res
    where xName = getVariableName x
          match = filter (\y -> getVariableName y == xName) globs
          matchVar = assert (length match > 0) head match
          varSize = assert (isArrVar matchVar) (getArrVarSize $ getArrVar matchVar)
          isDefined = isJust varSize
          sizeSet = redNumSet [] $ fromJust varSize
          definedRange = if isDefined then (if isJust sizeSet then fromJust sizeSet else error "failed to parse NumSet expr")  else error "Not defined"
          ditypeVar = head $ filter (\y -> getVariableName y == "ditype") consts
          ditypeNumSet = getCstNumSetVal $ getCstVar ditypeVar
          redDitypeNumSet = redNumSet [] ditypeNumSet
          finalRange = if isDefined then definedRange else if isJust redDitypeNumSet then (fromJust redDitypeNumSet) else error "Unable to parse itype"
          func2 i = VStruct (x, i, th)
          res = map func2 finalRange


allocateVarsByTh :: ThVarPair -> ProgramTree -> ProgramTree
allocateVarsByTh (th, vars) (LocLeaf (i, x, vs)) = if getPStructTh x == th then (ProgLeaf (-1, (LocLeaf (i, x, vs)), vars)) else LocLeaf (i, x, vs)
allocateVarsByTh pair (ProgLeaf (i, x, vars)) = ProgLeaf (i, allocateVarsByTh pair x, vars)
allocateVarsByTh pair (ProgNode (i, xs)) = ProgNode (i, map (allocateVarsByTh pair) xs)


calcUsedThread :: VariableStruct -> [ThVarPair] -> [VarName]
calcUsedThread var [] = []
calcUsedThread var ((th, vs):xs) = if varStructElem var vs then th:remain else remain
    where remain = calcUsedThread var xs

cntVarOfTree :: ProgramTree -> Int
cntVarOfTree (LocLeaf (_, _, vars)) = length vars
cntVarOfTree (ProgLeaf (_, p, vars)) = (cntVarOfTree p) + (length vars)
cntVarOfTree (ProgNode (_, xs)) = sum $ map cntVarOfTree xs


-- input : variable and a list of threads that access it
-- output: bool to indicate if write successfully
minAssignVarWTh :: (ProgramTree -> Int) -> (VariableStruct, [VarName]) -> ProgramTree -> (Bool, ProgramTree)
minAssignVarWTh calc (var, ths) (LocLeaf (i, struct, vars)) = (canAdd, res)
    where th = getPStructTh struct
          canAdd = elem th ths
          res = if canAdd then ProgLeaf (-1, (LocLeaf (i, struct, vars)), [var]) else (LocLeaf (i, struct, vars))
minAssignVarWTh calc (var, ths) (ProgLeaf (i, tree, vars)) = (b, res)
    where (b, subTree) = minAssignVarWTh calc (var, ths) tree
          res = if b then ProgLeaf (i, tree, var:vars) else ProgLeaf (i, tree, vars)
minAssignVarWTh calc (var, ths) (ProgNode (i, trees)) = if canAdd then (canAdd, res) else (canAdd, (ProgNode (i, trees)))
    where VStruct (_, ii, _) = var
          trees' = map (minAssignVarWTh calc (var, ths)) trees
          tuples = zip [0..((length trees)-1)] trees'
          validTu = filter (\(i,(b, x)) -> b) tuples
          canAdd = length validTu > 0
          mini = getMinElem (\(i,(b, x)) -> calc x) validTu
          idx = fst mini
          resTrees = (take idx trees) ++ [snd $ snd $ mini] ++ (drop (idx+1) trees)
          res = ProgNode (i, resTrees)



-- allocation method
-- first allocate to the ones with unique write access, then the ones with unique read access,
-- then randomly if multiple write (minimize size of subtree), then randomly if multiple read (minimize size of subtree)
allocateVars :: [Variable] -> [Variable] -> ([ThVarPair], [ThVarPair]) -> Program -> Program
allocateVars globs consts (wMap', rMap') prog = Prog (name, method, treeWMultiRead)
    where (wUnique, rUnique) = calcUniqueVars globs consts (wMap', rMap')
          combine = \(x, y) -> (fst x, (snd x) ++ (snd y))
          allocated = map combine $ zip wUnique rUnique
          Prog (name, method, tree) = prog
          tree' = foldr allocateVarsByTh tree allocated
          flatAllocated = joinVarStructMap allocated
          allVars = removeDuplicate $ (joinVarStructMap rMap') ++ (joinVarStructMap wMap')
          unAllocated = filter (\y -> not $ varStructElem y flatAllocated) allVars
          unAllocWPair = zip unAllocated $ map (\x -> calcUsedThread x wMap') unAllocated 
          varWMultiWrite = filter (\(x, y) -> length y > 0) unAllocWPair 
          treeWMultiWrite = foldr (\x y -> snd $ minAssignVarWTh cntVarOfTree x y) tree' varWMultiWrite
          tree1 = minAssignVarWTh cntVarOfTree (varWMultiWrite!!0) tree' 
          tree2 = minAssignVarWTh cntVarOfTree (varWMultiWrite!!1) (snd tree1)
          remain = filter (\(x, y) -> length y == 0) unAllocWPair -- this should correspond to the ones with multiple read only
          treeWMultiRead = foldr (\x y -> snd $ minAssignVarWTh cntVarOfTree x y) treeWMultiWrite remain

