module Variable where

import IExpr
import BExpr
import Helper
import Token
import NumSet
import Keyword

import Control.Exception
import Data.Either
import Data.Maybe

-- data type
data Variable = NomVar NomVar | ArrVar ArrVar | CstVar ConstVar deriving (Eq)
data VarType = INT | BOOL | LOCK | UnknownT deriving (Show, Eq)
type Dirty = Bool

data ArrVar = AVar (VarType, VarName, Dirty, VarSize, Range, ArrVal) deriving (Show, Eq)
data VarSize = KnownS NumSet | UnknownS deriving (Show, Eq)
data Range = KnownR NumSet | UnknownR deriving (Show, Eq)
data ArrVal = IAVal [IExpr] | BAVal [BExpr] | UnknownA deriving (Eq)

data ConstVar = ConstIV (VarName, IExpr) | ConstBV (VarName, BExpr) | ConstNumSet (VarName, NumSet) | ConstOther (VarName, String) deriving (Show, Eq)

data NomVar = NVar (VarType, VarName, Dirty, Range, NomVal) deriving (Show, Eq)
data NomVal = IVVal IExpr | BVVal BExpr | UnknownV deriving (Eq)


instance Show Variable where
    show var = getVariableName var
    
instance Show ArrVal where
      show (IAVal xs) = "<" ++ str ++ ">"
            where str = join "," $ map show xs
      show (BAVal xs) = "<" ++ str ++ ">"
            where str = join "," $ map show xs
      show UnknownA = ""

instance Show NomVal where
      show (IVVal xs) = show xs
      show (BVVal xs) = show xs
      show UnknownV = ""

-- Variable

isNomVar :: Variable -> Bool
isNomVar (NomVar _) = True
isNomVar _ = False

isArrVar :: Variable -> Bool
isArrVar (ArrVar _) = True
isArrVar _ = False

isCstVar :: Variable -> Bool
isCstVar (CstVar _) = True
isCstVar _ = False

getNomVar :: Variable -> NomVar
getNomVar (NomVar x) = x
getNomVar _ = error "Invalid variable type: single variable expected"

getArrVar :: Variable -> ArrVar
getArrVar (ArrVar x) = x
getArrVar _ = error "Invalid variable type: array expected"

getCstVar :: Variable -> ConstVar
getCstVar (CstVar x) = x
getCstVar _ = error "Invalid variable type: constant expected"


-- VarType

getNomVarType :: NomVar -> VarType
getNomVarType (NVar (tp, _, _, _, _)) = tp

getArrVarType :: ArrVar -> VarType
getArrVarType (AVar (tp, _, _, _, _, _)) = tp

getCstVarType :: ConstVar -> VarType
getCstVarType (ConstBV _) = BOOL
getCstVarType (ConstIV _) = INT
getCstVarType _ = UnknownT

getVariableType :: Variable -> VarType
getVariableType (NomVar x) = getNomVarType x
getVariableType (ArrVar x) = getArrVarType x
getVariableType (CstVar x) = getCstVarType x


isIntVar :: Variable -> Bool
isIntVar x = (getVariableType x) == INT

isBoolVar :: Variable -> Bool
isBoolVar x = (getVariableType x) == BOOL

isLockVar :: Variable -> Bool
isLockVar x = (getVariableType x) == LOCK

-- VarName
getNomVarName :: NomVar -> VarName
getNomVarName (NVar (_, n, _, _, _)) = n

getArrVarName :: ArrVar -> VarName
getArrVarName (AVar (_, n, _, _, _, _)) = n

getCstVarName :: ConstVar -> VarName
getCstVarName (ConstBV (x, _)) = x
getCstVarName (ConstIV (x, _)) = x
getCstVarName (ConstNumSet (x, _)) = x
getCstVarName (ConstOther (x, _)) = x


getVariableName :: Variable -> VarName
getVariableName (NomVar x) = getNomVarName x
getVariableName (ArrVar x) = getArrVarName x
getVariableName (CstVar x) = getCstVarName x

-- isDirty

isDirtyNomVar :: NomVar -> Bool
isDirtyNomVar (NVar (_, _, b, _, _)) = b

isDirtyArrVar :: ArrVar -> Bool
isDirtyArrVar (AVar (_, _, b, _, _, _)) = b

isDirtyVar :: Variable -> Bool
isDirtyVar (NomVar x) = isDirtyNomVar x
isDirtyVar (ArrVar x) = isDirtyArrVar x

-- VarSize

getArrVarSize :: ArrVar -> Maybe NumSet
getArrVarSize (AVar (_, _, _, KnownS x, _, _)) = Just x
getArrVarSize (AVar (_, _, _, UnknownS, _, _)) = Nothing

-- Range

getNomVarRange :: NomVar -> Maybe NumSet
getNomVarRange (NVar (_, _, _, KnownR x, _)) = Just x
getNomVarRange (NVar (_, _, _, UnknownR, _)) = Nothing

getArrVarRange :: ArrVar -> Maybe NumSet
getArrVarRange (AVar (_, _, _, _, KnownR x, _)) = Just x
getArrVarRange (AVar (_, _, _, _, UnknownR, _)) = Nothing

getVariableRange :: Variable -> Maybe NumSet
getVariableRange (NomVar x) = getNomVarRange x
getVariableRange (ArrVar x) = getArrVarRange x

-- InitVal

getNomInitVal :: NomVar -> NomVal
getNomInitVal (NVar (_, _, _, _, v)) = v

getArrInitVal :: ArrVar -> ArrVal
getArrInitVal (AVar (_, _, _, _, _, v)) = v


-- overwrite

overwriteDirty :: Bool -> Variable -> Variable
overwriteDirty b (NomVar (NVar (tp, n, d, r, iv))) = NomVar (NVar (tp, n, b, r, iv))
overwriteDirty b (ArrVar (AVar (tp, n, d, s, r, iv))) = (ArrVar (AVar (tp, n, b, s, r, iv)))

overwriteRange :: Range -> Variable -> Variable
overwriteRange range (ArrVar (AVar (tp, n, d, s, r, iv))) = (ArrVar (AVar (tp, n, d, s, range, iv)))

overwriteSize :: VarSize -> Variable -> Variable
overwriteSize size (ArrVar (AVar (tp, n, d, s, r, iv))) = (ArrVar (AVar (tp, n, d, size, r, iv)))

overwriteName :: VarName -> Variable -> Variable
overwriteName name (NomVar (NVar (tp, n, d, r, iv))) = NomVar (NVar (tp, name, d, r, iv))
overwriteName name (ArrVar (AVar (tp, n, d, s, r, iv))) = (ArrVar (AVar (tp, name, d, s, r, iv)))

overwriteArrValue :: ArrVal -> ArrVar -> ArrVar
overwriteArrValue iv' (AVar (tp, n, d, s, r, iv)) = (AVar (tp, n, d, s, r, iv'))

overwriteNomValue :: NomVal -> NomVar -> NomVar
overwriteNomValue iv' (NVar (tp, n, d, r, iv)) = (NVar (tp, n, d, r, iv'))

renameVarByFunc :: (VarName -> VarName) -> Variable -> Variable
renameVarByFunc func var = overwriteName (func name) var
    where name = getVariableName var

-- newName

getNewVarName :: [VarName] -> VarName -> VarName
getNewVarName names base = head $ filter (\x -> not $ elem x names) xs
      where xs = iterIdxName base

iterIdxName :: VarName -> [VarName]
iterIdxName name = iterIdxName' name 1

iterIdxName' :: VarName -> Int -> [VarName]
iterIdxName' name n = (name ++ (show n)):(iterIdxName' name (n+1))

getTopLvlVarName :: [Tokentype] -> String
getTopLvlVarName xs = getSingleVarName l
        where (l, r) = break ((Special "[")==) xs

dummy :: Variable
dummy = NomVar (NVar (UnknownT, "dummy", False, UnknownR, UnknownV))

createLocalNomVar :: VarType -> VarName -> Variable
createLocalNomVar t name = NomVar (NVar (t, name, False, UnknownR, UnknownV))

createLocalArrVar :: VarType -> VarName -> Variable
createLocalArrVar t name = ArrVar (AVar (t, name, False, UnknownS, UnknownR, UnknownA))

createCstVar :: VarName -> Variable
createCstVar name = CstVar (ConstOther (name, "dummy"))

-------- parse variable --------------------------------

-- main function

getVarLineCmd :: [Tokentype] -> [Variable]
getVarLineCmd xs | (last xs == (Special ";")) = getVarLineCmd' $ init xs
                 | otherwise = getVarLineCmd' xs

getVarLineCmd' :: [Tokentype] -> [Variable]
getVarLineCmd' ((Keyword Dirty):xs) = map (overwriteDirty True) $ getVarLineCmd' xs
getVarLineCmd' ((Keyword Integer):xs) = func (xs)
            where func = if (elem (Special "[") xs) then getVarIACmd else getVarIVCmd
getVarLineCmd' ((Keyword Boolean):xs) = func (xs)
            where func = if (elem (Special "[") xs) then getVarBACmd else getVarBVCmd
getVarLineCmd' ((Keyword LockK):xs) = func (xs)
            where func = if (elem (Special "[") xs) then getVarLACmd else getVarLVCmd



-- functions to get each field from tokens

getVarRange :: [Tokentype] -> Range
getVarRange xs = if (topLvlParenthesisElem (==(Special "%")) xs) then KnownR (getDefaultSet val) else UnknownR
      where idx = matchIdx xs [Special "%"]
            val = strToInt $ getTokenValue $ xs!!(idx+1)

getVarSize :: [Tokentype] -> VarSize
getVarSize xs = if (lIdx +1 == rIdx) then UnknownS else (if (lIdx + 2 == rIdx) then KnownS (getDefaultSet val) else error "Error when declaring array variable size") 
        where lIdx = matchIdx xs [Special "["]
              rIdx = matchIdx xs [Special "]"]
              val = strToInt $ getTokenValue $ xs!!(lIdx +1)

-- the first argument indicates if this is int
getNomVal :: VarType -> [Tokentype] -> NomVal
getNomVal _ [] = UnknownV
getNomVal INT ys = IVVal $ (getIExpr ys)
getNomVal BOOL ys = BVVal $ (getBExpr ys)
getNomVal LOCK ys = BVVal $ (getBExpr ys)

getArrVal :: VarType -> [Tokentype] -> ArrVal
getArrVal _ [] = UnknownA
getArrVal tp ((Special "{"):xs) = assert checkFormat res
    where checkFormat = (last xs) == (Special "}")
          parts = topLvlParenthesisSplit (==(Special ",")) $ init xs
          res = if tp==INT then IAVal $ map getIExpr parts else BAVal $ map getBExpr parts -- boolean for lock case as well
          

getVarArrCmd :: VarType -> [Tokentype] -> [Variable]
getVarArrCmd tp xs = map (overwriteRange range) $ map (overwriteSize varSize) $ getVarArrCmd' tp $ tail r
          where range = getVarRange xs
                (l, r) = break (==(Special "]")) xs
                varSize = getVarSize xs
getVarArrCmd' _ [] = []
getVarArrCmd' tp xs = (ArrVar $ AVar (tp, varName, False, UnknownS, UnknownR, initVal)):(getVarArrCmd' tp r)
          where (l, r) = topLvlParenthesisBreak (==(Special ",")) xs
                (l', r') = topLvlParenthesisBreak (==(Special "=")) l
                varName = getSingleVarName l'
                initVal = getArrVal tp r'

-- this should not include overwrite range because integer should always use default range
getVarCmd :: VarType -> [Tokentype] -> [Variable]
getVarCmd tp xs = getVarCmd' tp xs

getVarCmd' _ [] = []
getVarCmd' tp xs = (NomVar $ NVar (tp, varName, False, range, initVal)):(getVarCmd' tp r)
          where (l, r) = topLvlParenthesisBreak (==(Special ",")) xs
                (l', r') = topLvlParenthesisBreak (==(Special "=")) l
                range = getVarRange xs
                varName = getSingleVarName l'
                initVal = getNomVal tp r'

getVarBACmd :: [Tokentype] -> [Variable]
getVarBACmd = getVarArrCmd BOOL

getVarIACmd :: [Tokentype] -> [Variable]
getVarIACmd = getVarArrCmd INT

getVarLACmd :: [Tokentype] -> [Variable]
getVarLACmd = getVarArrCmd LOCK

getVarBVCmd :: [Tokentype] -> [Variable]
getVarBVCmd = getVarCmd BOOL

getVarIVCmd :: [Tokentype] -> [Variable]
getVarIVCmd = getVarCmd INT

getVarLVCmd :: [Tokentype] -> [Variable]
getVarLVCmd = getVarCmd LOCK

-- try to get all global and local variables

-- show try to ignore the scope and find all statements starting with Int/Bool

getTokensForVariable :: [Tokentype] -> [[Tokentype]]
getTokensForVariable xs = getTokensForVariable' [] [] False xs

getTokensForVariable' s1 s2 _ [] = s1:s2
getTokensForVariable' s1 s2 False (x:xs) | elem x [Keyword Integer, Keyword Boolean] = getTokensForVariable' [x] s2 True xs
                                         | otherwise = getTokensForVariable' s1 s2 False xs
getTokensForVariable' s1 s2 True (x:xs)  | elem x list = getTokensForVariable' [] ((reverse s1):s2) False xs
                                         | otherwise = getTokensForVariable' (x:s1) s2 True xs
        where list = [Special ";", Special "//", Special "{", Special "\n", Special "}"]


getLocalVariable :: [Tokentype] -> [Variable]
getLocalVariable xs =  foldr (\x y -> (getVarLineCmd x)++y) [] ls'
        where ls = getTokensForVariable xs
              ls' = filter (\x -> (length x) > 0) ls

----- check for cstVar
-- data ConstVar = ConstIV (VarName, IExpr) | ConstBV (VarName, BExpr) | ConstNumSet (VarName, NumSet) | ConstOther (VarName, String) deriving (Show, Eq)

isConstIV :: ConstVar -> Bool
isConstIV (ConstIV _) = True
isConstIV _ = False

isConstBV :: ConstVar -> Bool
isConstBV (ConstBV _) = True
isConstBV _ = False

isCstNumSet :: ConstVar -> Bool
isCstNumSet (ConstNumSet _) = True
isCstNumSet _ = False

isCstOther :: ConstVar -> Bool
isCstOther (ConstOther _) = True
isCstOther _ = False

getCstIVVal :: ConstVar -> IExpr
getCstIVVal (ConstIV (x, v)) = v

getCstBVVal :: ConstVar -> BExpr
getCstBVVal (ConstBV (x, v)) = v

getCstNumSetVal :: ConstVar -> NumSet
getCstNumSetVal (ConstNumSet (x, v)) = v

getCstOtherVal :: ConstVar -> String
getCstOtherVal (ConstOther (x, v)) = v

getVarPrefix :: Variable -> String
getVarPrefix (NomVar var) = if isIntVar (NomVar var) then "IV" else "BV"
getVarPrefix (ArrVar var) = if isIntVar (ArrVar var) then "IA" else "BA"
getVarPrefix _ = error "expecting a variable"

-- set representation of vars
getVarString :: Variable -> String
getVarString (NomVar var) = "{" ++ (join "." [prefix, name]) ++"}"
      where prefix = getVarPrefix (NomVar var)
            name = getNomVarName var
getVarString (ArrVar var) = "{" ++ (join "." [prefix, name, "i"]) ++ "| i <-itype("++ name++")}"
      where prefix = getVarPrefix (ArrVar var)
            name = getArrVarName var
getVarString _ = error "expecting a variable"
