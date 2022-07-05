module ProcLine where

import Helper
import Token
import Variable
import AstProc
import CSPProc
import ProgramStruct
import Overall
import Signal
import Environment

isProcLine :: String -> Bool
isProcLine s = (topLvlParenthesisElem (==(Special "=")) xs) && (length l >= 3) && (last l == (Special ")")) && (l!!1 == (Special "("))
        where xs = tokenize s
              (l, r) = topLvlParenthesisBreak (==(Special "=")) xs


getRawCmdWLocal :: String -> [Variable] -> [Variable] -> ProcWLocal
getRawCmdWLocal s globals consts = CmdWLocal (name, ps', cmd, locals, [])
        where xs = tokenize s
              renamedTokens = renameProcToken xs
              locals = getLocalVariable renamedTokens
              (ProcDef (name, ps, cmd)) = getProc globals consts xs
              th = "th"
              ps' = ps ++ [th]

getRawCmdWLocals :: [String] -> [Variable] -> [Variable] -> [ProcWLocal]
getRawCmdWLocals (x:xs) gloabls consts  | isProcLine x = (getRawCmdWLocal x gloabls consts):(getRawCmdWLocals xs gloabls consts)
                                        | otherwise = getRawCmdWLocals xs gloabls consts
getRawCmdWLocals [] _ _ = []



printSingleProc :: [Variable] -> [Variable] -> [SigDeclar] -> ProcWLocal -> String
printSingleProc globals consts signals procWLocal = show procDef
    where CmdWLocal (name, ps, cmd, locals, _) = procWLocal
          psConsts = map createCstVar ps
          env = Env (globals, consts ++ psConsts, locals)
          (env1, cspproc) = mainProc env cmd
          gp = last $ init ps
          th = last ps
          cspproc1 = overwriteCSPProcTh th cspproc
          sigName = map getSignalName signals
          cspproc2 = removeSigProcTh sigName cspproc1
          procDef = CSPDef (name, ps, cspproc2)

checkOneNeedAtomic :: ProcWLocal -> Bool
checkOneNeedAtomic (CmdWLocal (_, _, cmd, _, _)) = checkCmdNeedAtomic cmd

checkAllNeedAtomic :: [ProcWLocal] -> Bool
checkAllNeedAtomic xs = foldr (||) False $ map checkOneNeedAtomic xs

printChecAtomicLine :: [ProcWLocal] -> String
printChecAtomicLine xs = join "" ["needVarAt = ", condStr]
      where cond = checkAllNeedAtomic xs
            condStr = if cond then "true" else "false"


printOverallProc :: Overall -> String
printOverallProc (Overall (globals, consts, procs, asserts, signals, progs)) = join "\n\n" [needAtomicStr, str]
    where str =  join "\n" $ map (printSingleProc globals consts signals) procs
          needAtomicStr = printChecAtomicLine procs
