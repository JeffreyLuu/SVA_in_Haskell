module Environment where

import Variable
import Helper

import Data.Maybe

-- global variables, local variables, constants
data Env = Env ([Variable], [Variable], [Variable]) deriving (Show)

envAllVars :: Env -> [Variable]
envAllVars (Env (xs, ys, zs)) = xs++ys++zs

envAllVarName :: Env -> [VarName]
envAllVarName env = map getVariableName $ envAllVars env

getNewNameWEnv :: Env -> VarName -> VarName
getNewNameWEnv env base = getNewVarName arr base
      where arr = envAllVarName env

-- checkVarNameType :: VarName -> Env -> VarType
-- checkVarNameType name (Env (xs, ys, zs)) = if (length filteredVar > 0) then (getVariableVarType $ head filteredVar) else if (length filteredConst > 0) then CONST else UnknownT
--         where filteredVar = sameNameVar (ys ++ xs) name
--               filteredConst = filter (\x -> name == getVariableVarName x) zs

-- changed so that returns Int/bool iff it is a var
checkVarNameType :: VarName -> Env -> VarType
checkVarNameType name (Env (xs, ys, zs)) = if (length filteredVar > 0) then (getVariableType $ head filteredVar) else UnknownT
      where filteredVar = sameNameVar (xs++ys) name
            -- filteredConst = filter (\x -> name == getVariableName x) zs

sameNameVar :: [Variable] -> VarName -> [Variable]
sameNameVar xs name = filter (\x -> (getVariableName x) == name) xs

filterVariable :: VarName -> Env -> Maybe Variable
filterVariable name (Env (xs, ys, zs)) = if (length filteredVar >0) then (Just $ head filteredVar) else Nothing
        where filteredVar = sameNameVar (xs++ys++zs) name

filterConstant :: VarName -> Env -> Maybe Variable
filterConstant name (Env (_, _, zs)) = if (length filteredVar >0) then (Just $ head filteredVar) else Nothing
        where filteredVar = sameNameVar zs name
            
addLocalVar :: Variable -> Env -> Env
addLocalVar y (Env (xs, ys, zs)) = Env (xs, y:ys, zs)

addLocalVars :: [Variable] -> Env -> Env
addLocalVars xs env = foldr addLocalVar env xs

addConstName :: VarName -> Env -> Env
addConstName z env = addConstVar var env
      where var = CstVar (ConstOther (z, "Unknown Constant"))--Variable (CONST, z, False, UnknownS, UnknownR, UnknownV)

addConstNames :: [VarName] -> Env -> Env
addConstNames xs env = foldr addConstName env xs

addConstVar :: Variable -> Env -> Env
addConstVar z (Env (xs, ys, zs)) = Env (xs, ys, z:zs)
