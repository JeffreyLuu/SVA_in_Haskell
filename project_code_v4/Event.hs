module Event where

import IExpr
import BExpr
import Helper
import Variable

import Control.Exception
import Data.Maybe


data FieldType = Output | Input | NoField deriving (Eq)
data Thread = YesTh (FieldType, VarName) | NoTh deriving (Eq)
data Location = YesLocV (FieldType, Variable) | YesLocA (FieldType, Variable, IExpr) | YesLocSig (FieldType, [IExpr]) | NoLoc deriving (Eq)
data EventValue = YesIVal (FieldType, IExpr) | YesBVal (FieldType, BExpr) | YesLVal (FieldType, Bool) |  NoVal deriving (Eq)
data Event = Econ (VarName, Thread, Location, EventValue) | Err (Errors, Thread) | DummyEvt (String, String) deriving (Eq)


instance Show FieldType where 
    show Output = "!"
    show Input = "?"
    show NoField = ""

instance Show Thread where
    show (YesTh (tp, i)) = (show tp) ++ i
    show NoTh = ""

instance Show Location where
    show (YesLocV (tp, v)) = (show tp) ++ (getLocVarStr (YesLocV (tp, v)))
    show (YesLocA (tp, a, iexp)) = (show tp) ++ (getLocVarStr (YesLocA (tp, a, iexp)))
    show (YesLocSig (tp, xs)) = if str == "" then "" else (show tp) ++ str
        where str = join "." $ map show xs
    show NoLoc = ""

instance Show EventValue where
    show (YesIVal (tp, iexp)) = (show tp) ++ (show iexp)
    show (YesBVal (tp, bexp)) = (show tp) ++ (show bexp)
    show (YesLVal (tp, bool)) = (show tp) ++ str
        where str = if bool then "lock" else "unlock"
    show NoVal = ""

instance Show Event where
    show (Econ (name, th, loc, value)) = name ++ (show th) ++ (show loc) ++ (show value)
    show (Err (Error, th)) = "error" ++ (show th)
    show (Err (VError, th)) = "verror" ++ (show th)

getLocVarStr :: Location -> String
getLocVarStr (YesLocV (_, var)) = prefix ++ (show var)
    where isInt = isIntVar var
          isBool = isBoolVar var
          prefix = if isInt then "IV." else if isBool then "BV." else "LV."
getLocVarStr (YesLocA (_, var, iexp)) = prefix ++ (show var) ++ "." ++ (show iexp')
    where isInt = isIntVar var
          isBool = isBoolVar var
          prefix = if isInt then "IA." else if isBool then "BA." else "LA."
          iexp' = if needParamInt iexp then IPARA iexp else iexp
getLocVarStr _ = error "Not implemented, this should only be called for variables"


isLocA :: Location -> Bool
isLocA (YesLocA _) = True
isLocA _ = False

getLocVar :: Location -> Variable
getLocVar (YesLocV (_, var)) = var
getLocVar (YesLocA (_, var, _)) = var
getLocVar x = error $ "This location contains no var" ++ (show x)

itypeStr :: Location -> String
itypeStr (YesLocA (tp, v, exp)) = "itype(" ++ (show v) ++ ")"
itypeStr x = error $ "Undefined cases in itypeStr" ++ (show x)

ctypeStr :: Location -> String
ctypeStr (YesLocV (tp, v)) = "ctype(" ++ (getLocVarStr (YesLocV (tp, v))) ++ ")"
ctypeStr (YesLocA (tp, v, exp)) = "ctype(" ++ (getLocVarStr (YesLocA (tp, v, exp))) ++ ")"
ctypeStr x = error $ "Undefined cases in ctypeStr" ++ (show x)


getLocIA :: FieldType -> Variable -> IExpr -> Location
getLocIA tp var (IArc (name, iexpr)) = YesLocA (tp, var, iexpr)
getLocIA tp var _ = error "Expecting IArc for IExpr"

getLocBA :: FieldType -> Variable -> BExpr -> Location
getLocBA tp var (BArc (name, iexpr)) = YesLocA (tp, var, iexpr)
getLocBA tp var _ = error "Expecting BArc for BExpr"

getEvenName :: Event -> VarName
getEvenName (Econ (n, _, _, _)) = n

isErrEvent :: Event -> Bool
isErrEvent (Err x) = True
isErrEvent _ = False


overwriteEventTh :: FieldType -> VarName -> Event -> Event
overwriteEventTh NoField thName (Econ (name, _, loc, value)) = Econ (name, (NoTh), loc, value)
overwriteEventTh tp thName (Econ (name, _, loc, value)) = Econ (name, (YesTh (tp, thName)), loc, value)
overwriteEventTh NoField thName (Err (err, _)) = error "Invalid thread field to be overwritten with"
overwriteEventTh tp thName (Err (err, _)) = Err (err, YesTh (tp, thName))


removeEventTh :: Event -> Event
removeEventTh (Econ (name, _, loc, value)) = Econ (name, NoTh, loc, value)


getEventValIExpr :: EventValue -> IExpr
getEventValIExpr (YesIVal (_, iexp)) = iexp
getEventValIExpr _ = error "Invalid input when expecting YesIVal"


getChanName :: VarType -> VarName
getChanName INT = "IV_Chan"
getChanName BOOL = "BV_Chan"
getChanName _ = error "Invalid type to get chan"


getThreadName :: Thread -> Maybe VarName
getThreadName (YesTh (_, x)) = Just x
getThreadName NoTh = Nothing

getEventLocation :: Event -> Maybe Location
getEventLocation (Econ (_, _, loc, _)) = Just loc
getEventLocation (Err _) = Nothing

getLocVariablee :: Location -> Maybe Variable
getLocVariablee (YesLocV (_, var)) = Just var
getLocVariablee (YesLocA (_, var, _)) = Just var
getLocVariablee _ = Nothing

