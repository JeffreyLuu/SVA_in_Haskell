module Parser where

import Helper
import Token
import VarLine
import ConstVar
import ConstLine
import ProcLine
import Assertion
import Signal
import SignalLine
import ProgramStruct
import ProgramLine
import Variable
import Overall

parse :: String -> [String]
parse contents = splitByPairUp $ map removeComment $ lines contents


removeComment :: String -> String
removeComment s | (length s >= 2 && take 2 s == "//") = ""
                | (length s >= 2 && take 2 s == "--") = ""
                | (length s == 0) = ""
                | otherwise = (head s):(removeComment $ tail s)


splitByPairUp :: [String] -> [String]
splitByPairUp [] = []
splitByPairUp [l] = [l]
splitByPairUp (l1:l2:ls)  | invalidStartOfLine l2 = (splitByPairUp ((l1 ++ "\n" ++ l2):ls))
                          | thisCnt == 0 = l1:(splitByPairUp (l2:ls))
                          | otherwise = (splitByPairUp ((l1 ++ "\n" ++ l2):ls))
        where count sym xs = length $ filter (==sym) xs
              curlyCnt s = (count '{' s) - (count '}' s)
              invalidStartOfLine ('{':_) = True
              invalidStartOfLine _ = False
              thisCnt = curlyCnt l1

----------------------------------------

-- instance Show Overall where
--     show overall = join "\n\n" [varLine, constLine, sigLine, procLine, programLine]
--         where varLine = printAllVariables overall
--               constLine = printOverallConstants overall
--               sigLine = printOverallSignal overall
--               procLine = printOverallProc overall
--               programLine = printOverallProgram overall
--               Overall (globals, consts, procs, asserts, signals, progs) = overall

instance Show Overall where
    show overall = join "\n\n" [varLine, constLine, procLine, sigLine, programLine, assertions]
        where varLine = printAllVarLines overall
              constLine = printAllCstLines overall
              procLine = printOverallProc overall
              assertions = join "\n" $ map show asserts
              sigLine = printSignalLines signals
              programLine = printOverallProgram overall
              Overall (globals, consts, procs, asserts, signals, progs) = overall

rawCompileAll :: [String] -> Overall
rawCompileAll xs = Overall (globals, consts, rawProcs, assertions, signals, programs)
    where globals = getAllVariables xs
          consts = redConstVars $ getRawGlobalConstants xs
          rawProcs = getRawCmdWLocals xs globals consts
          assertions = getAssertions xs
          signals = getSigDeclar xs
          programs = getPrograms rawProcs xs
