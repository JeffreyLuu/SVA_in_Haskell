module ConstLine where

import Helper
import Token
import Variable
import Overall

import Control.Exception

getOverallConstants :: Overall -> [Variable]
getOverallConstants (Overall (globals, consts, procs, asserts, signals, progs)) = consts

printCstLine :: ConstVar -> String
printCstLine (ConstIV (name, exp)) = name ++ " = " ++ (show exp)
printCstLine (ConstBV (name, exp)) = name ++ " = " ++ (show exp)
printCstLine (ConstNumSet (name, exp)) = name ++ " = " ++ (show exp)
printCstLine (ConstOther (name, exp)) = name ++ " = " ++ exp


printAllCstLines :: Overall -> String
printAllCstLines overall = join "\n" cstStr
    where csts = getOverallConstants overall
          cstStr = map printCstLine $ map getCstVar csts
