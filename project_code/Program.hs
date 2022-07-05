module Program where

import ProgramStruct
import Variable
import Helper

type ThVarPair = (VarName, [VariableStruct])
type VarThPair = (VariableStruct, [VarName])

joinVarStructMap :: [ThVarPair] -> [VariableStruct]
joinVarStructMap xs = foldr (++) [] $ map snd xs

findUniqueAccess :: [ThVarPair] -> [ThVarPair]
findUniqueAccess mapA = resMap
    where joinAll = \xs -> foldr (++) [] $ map snd xs
          func = \i -> (take (i-1) mapA) ++ (drop i mapA)
          others = map joinVarStructMap $ map func [1..(length mapA)]
          pairUp = zip mapA others
          func2 = \((th, xs), ys) -> (th, filter (\y -> not $ varStructElem y ys) xs)
          resMap = map func2 pairUp


printThVarPair :: String -> Int -> ThVarPair -> String
printThVarPair head pid (th, vars) = join "" [head, "(", show pid, ",", th, ") = {", varStr, "}"]
    where varStr = join "," $ map show vars

