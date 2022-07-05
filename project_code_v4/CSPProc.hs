module CSPProc where

import Event
import Helper
import IExpr
import BExpr
import AstProc
import Variable
import Environment
import ProgramStruct

import Control.Exception
import Data.Maybe

-- data type

data CSPProc = SKIP | STOP | 
              CONC (Event, CSPProc)  |
              CSPSEQ (CSPProc, CSPProc) |
              APPL (ProcName, [VarName]) |
              LET (ProcName, [VarName], CSPProc, CSPProc) |
              CSPCON (BExpr, CSPProc, CSPProc) |
              EXTCHOICE (CSPProc, CSPProc)
              deriving (Eq)

errorSTOP :: CSPProc
errorSTOP = CONC (Err (Error, NoTh), STOP)

data CSPProcDef = CSPDef (VarName, [VarName], CSPProc) deriving (Eq)

-- instance Show CSPProc where
--     show SKIP = "SKIP"
--     show STOP = "STOP"
--     show (CONC (e, csp)) = (show e) ++ " -> " ++ (show csp)
--     show (CSPSEQ (a, b)) = "((" ++ (show a) ++ "); " ++ (show b) ++ ")"
--     show (APPL (name, paras)) = name ++ "(" ++ str ++ ")"
--         where str = join "," paras
--     show (LET (name, para, proc1, proc2)) = "let " ++ procName ++ " = (" ++ procBody ++ ") within " ++ finalProc
--         where procName = show (APPL (name, para))
--               procBody = show proc1
--               finalProc = show proc2 
--     show (CSPCON (con, a, b)) = "if " ++ (show con) ++ " then " ++ sa ++ " else " ++ sb
--         where sa = show a
--               sb = show b
--     show (EXTCHOICE (l, r)) = (show l) ++ " [] " ++ (show r)

instance Show CSPProc where
      show x = join "\n" $ map indent $ showCSPProcWIndent x

indent :: String -> String
indent s = "  "++s

singleLineCSPProc :: CSPProc -> Bool
singleLineCSPProc SKIP = True
singleLineCSPProc STOP = True
singleLineCSPProc (CONC _) = True
singleLineCSPProc _ = False

showCSPProcWIndent :: CSPProc -> [String]
showCSPProcWIndent SKIP = ["SKIP"]
showCSPProcWIndent STOP = ["STOP"]
showCSPProcWIndent (CONC (e, csp)) = if singleLineCSPProc csp then res1 else res2
      where tmp = showCSPProcWIndent csp
            res1 = [(show e) ++ " -> " ++ (head tmp)] ++ (tail tmp)
            res2 = [(show e) ++ " -> ("] ++ (map indent tmp) ++ [")"]
showCSPProcWIndent (CSPSEQ (a, b)) = if singleLine then ["(" ++ (head l) ++ ") ;"] ++ r else ["(" ++ (head l)] ++ (tail $ init l) ++ [(last l) ++ " );"] ++ r
      where l = showCSPProcWIndent a
            r = showCSPProcWIndent b
            singleLine = (length l == 1)
showCSPProcWIndent (APPL (name, paras)) = [name ++ "(" ++ str ++ ")"]
      where str = join "," paras
showCSPProcWIndent (LET (name, para, proc1, proc2)) = [headLine] ++ (map indent procBody) ++ [lastLine]
      where procName = head $ showCSPProcWIndent (APPL (name, para))
            procBody = showCSPProcWIndent proc1
            finalProc = head $ showCSPProcWIndent proc2 
            headLine = "let " ++ procName ++ " = ("
            lastLine = ") within " ++ finalProc
showCSPProcWIndent (CSPCON (con, a, b)) = res
      where sa = showCSPProcWIndent a
            sb = showCSPProcWIndent b
            fstSingle = length sa == 1
            sndSingle = length sb == 1
            ifPart = ["if " ++ (show con)]
            thenPart = if fstSingle then ["then " ++ (head sa)] else ["then ("] ++ (map indent sa) ++ [")"]
            elsePart = if sndSingle then ["else " ++ (head sb)] else ["else ("] ++ (map indent sb) ++ [")"]
            res = if fstSingle && sndSingle then [join " " $ ifPart ++ thenPart ++ elsePart]
                  else if sndSingle then ifPart ++ (init thenPart) ++ [(last thenPart) ++ " " ++ (head elsePart)]
                  else ifPart ++ thenPart ++ elsePart
showCSPProcWIndent (EXTCHOICE (l, r)) = [(show l) ++ " [] " ++ (show r)]

instance Show CSPProcDef where
    show (CSPDef (name, paras, pc)) = name ++ "(" ++ str ++ ")" ++ " = (\n" ++ (show pc) ++"\n)"
        where str = join "," paras

bExpEval2 :: Env -> BExpr -> ([Event], Env, BExpr)
bExpEval2 env BTrue = ([], env, BTrue)
bExpEval2 env BFalse = ([], env, BFalse)
bExpEval2 env (Not exp) = (a, env', Not b)
      where (a, env', b) = bExpEval2 env exp
bExpEval2 env (BPARA exp) = (a, env', BPARA b)
      where (a, env', b) = bExpEval2 env exp
bExpEval2 env (BVar name) = assert (getVariableType loc == BOOL) ([e], env', BVar newVarName)
      where variable = filterVariable name env
            loc = if isNothing variable then error "Undefined varible" else fromMaybe dummy variable
            newVarName = getNewNameWEnv env (name++"Val_")
            newVar = createLocalNomVar BOOL newVarName
            e = Econ ("bveval", NoTh, YesLocV (Output, loc), YesBVal (Input, BVar newVarName))
            env' = addLocalVar newVar env
bExpEval2 env (BArc (name, iexp)) = assert (getVariableType loc == BOOL) (a ++ [e], env2, BVar newVarName)
      where (a, env1, b) = iExpEval env iexp
            newVarName = getNewNameWEnv env1 (name++"Val_")
            newVar = createLocalArrVar BOOL newVarName
            b' = if needParamInt b then IPARA b else b
            variable = filterVariable name env
            loc = if isNothing variable then error "Undefined varible" else fromMaybe dummy variable
            e = Econ ("bveval", NoTh, YesLocA (Output, loc, b'), YesBVal (Input, BVar newVarName))
            env2 = addLocalVar newVar env1
bExpEval2 env (BBOp (op, l, r)) = (fetchedEvts, env2, BBOp (op, bl, br))
      where (al, env1, bl) = bExpEval2 env l
            (ar, env2, br) = bExpEval2 env1 r
            fetchedEvts = al++ar
bExpEval2 env (CompOp (op, l, r)) = (fetchedEvts, env2, CompOp (op, bl, br))
      where (al, env1, bl) = iExpEval env l
            (ar, env2, br) = iExpEval env1 r
            fetchedEvts = al++ar
bExpEval2 env ErrorB = ([Err (VError, NoTh)], env, ErrorB)

bLvEval :: Env -> BExpr -> ([Event], Env, BExpr)
bLvEval env (BVar name) = assert (elem name names) ([], env, BVar name) 
    where names = envAllVarName env
bLvEval env (BArc (name, iexp)) = (a, env', BArc (name, b))
      where (a, env', b) = iExpEval env iexp
bLvEval env ErrorB = ([Err (VError, NoTh)], env, ErrorB)
bLvEval env _ = ([], env, ErrorB)

bSubst :: (BExpr, BExpr) -> BExpr -> BExpr
bSubst (u,v) BTrue = BTrue
bSubst (u,v) BFalse = BFalse
bSubst (u,v) (Not x) = Not (bSubst (u,v) x)
bSubst (u,v) (BPARA exp) = BPARA (bSubst (u,v) exp)
bSubst (u,v) (BVar name) = if u==(BVar name) then v else BVar name
bSubst (u,v) (BArc (name, iexp)) = if u==(BArc (name, iexp)) then v else BArc (name,iexp)
bSubst (u,v) (BBOp (op, l, r)) = BBOp (op, bSubst (u,v) l, bSubst (u,v) r)
bSubst (u,v) (CompOp (op, l, r)) = CompOp (op, l, r)
bSubst (u,v) ErrorB = ErrorB

bSubsti :: (IExpr, IExpr) -> BExpr -> BExpr
bSubsti (u,v) BTrue = BTrue
bSubsti (u,v) BFalse = BFalse
bSubsti (u,v) (Not x) = Not (bSubsti (u,v) x)
bSubsti (u,v) (BPARA exp) = BPARA (bSubsti (u,v) exp)
bSubsti (u,v) (BVar name) = BVar name
bSubsti (u,v) (BArc (name, iexp)) = BArc (name, iSubst (u,v) iexp)
bSubsti (u,v) (BBOp (op, l, r)) = BBOp (op, bSubsti (u,v) l, bSubsti (u,v) r)
bSubsti (u,v) (CompOp (op, l, r)) = CompOp (op, iSubst (u,v) l, iSubst (u,v) r)
bSubsti (u,v) ErrorB = ErrorB

bFetch :: Env -> BExpr -> (Maybe IExpr, Maybe BExpr)
bFetch env BTrue = (Nothing, Nothing)
bFetch env BFalse = (Nothing, Nothing)
bFetch env (Not x) = bFetch env x
bFetch env (BPARA x) = bFetch env x
bFetch env (BVar name) = if isCst then (Nothing, Nothing) else (Nothing, Just (BVar name))
      where variable = filterVariable name env
            var = if isJust variable then fromJust variable else error "Unseen variable"
            isCst = isCstVar var
bFetch env (BArc (name, e)) = if isJust e' then (e', Nothing) else (Nothing, Just (BArc (name, e)))
      where e' = iFetch env e
bFetch env (BBOp (op, l, r)) = if l' == (Nothing, Nothing) then r' else l'
      where l' = bFetch env l
            r' = bFetch env r
bFetch env (CompOp (op, l, r)) = if l' == Nothing then (r', Nothing) else (l', Nothing)
      where l' = iFetch env l
            r' = iFetch env r
bFetch env ErrorB = (Nothing, Nothing)

bExpEval1 :: Env -> BExpr -> ([Event], Env, BExpr)
bExpEval1 env exp = if nothing then ([], env, exp) else (e:evts, env2, exp2)
      where sub = bFetch env exp
            nothing = sub == (Nothing, Nothing)
            subInt = isJust $ fst sub
            name = if subInt then getIExprVarName $ fromJust $ fst sub else getBExprVarName $ fromJust $ snd sub
            variable = filterVariable name env
            var = if isJust variable then fromJust variable else error "Unseen variable"
            newVarName = getNewNameWEnv env (name++"Val_")
            newVar = createCstVar newVarName
            e = if subInt then convertToIFetchEvt (fromJust $ fst sub) var newVarName else convertToBFetchEvt (fromJust $ snd sub) var newVarName
            env1 = addLocalVar newVar env
            exp1 = if subInt then bSubsti (fromJust $ fst sub, IVar newVarName) exp else bSubst (fromJust $ snd sub, BVar newVarName) exp 
            (evts, env2, exp2) = bExpEval1 env1 exp1

-- iExpEval
bExpEval = bExpEval1
iExpEval = iExpEval1


iExpEval2 :: Env -> IExpr -> ([Event], Env, IExpr)
iExpEval2 env (Const x) = ([], env, Const x)
iExpEval2 env (IPARA exp) = (a, env', IPARA b)
      where (a, env', b) = iExpEval2 env exp
iExpEval2 env (IVar name) = resExpr
      where variable = filterVariable name env
            Env (_, _, zs) = env
            loc = if isNothing variable then (if elem name (map getVariableName zs) then createCstVar name else error $ "undefined variable" ++ name ++ show env) else fromMaybe dummy variable
            newVarName = getNewNameWEnv env (name++"Val_")
            newVar = createLocalNomVar INT newVarName
            e = Econ ("iveval", NoTh, YesLocV (Output, loc), YesIVal (Input, IVar newVarName))
            env' = addLocalVar newVar env
            varType = getVariableType loc
            isCst = isCstVar loc
            resExpr = if isCst then ([], env, IVar name) else if varType == INT then ([e], env', IVar newVarName) else error "Unseen type"
iExpEval2 env (IArc (name, iexp)) = assert (getVariableType loc == INT) (a ++ [e], env2, IVar newVarName)
      where (a, env1, b) = iExpEval2 env iexp
            newVarName = getNewNameWEnv env1 (name++"Val_")
            newVar = createLocalArrVar INT newVarName
            b' = if needParamInt b then IPARA b else b
            variable = filterVariable name env
            Env (_, _, zs) = env
            loc = if isNothing variable then (if elem name (map getVariableName zs) then createCstVar name else error "undefined variable") else fromMaybe dummy variable
            e = Econ ("iveval", NoTh, YesLocA (Output, loc, b'), YesIVal (Input, IVar newVarName))
            env2 = addLocalVar newVar env1
iExpEval2 env (BIOp (op, l, r)) = (fetchedEvts, env2, BIOp (op, bl, br))
      where (al, env1, bl) = iExpEval2 env l
            (ar, env2, br) = iExpEval2 env1 r
            check = if (elem op [Div, Mod]) then [DummyEvt ("check_zero", show br)] else []
            fetchedEvts = (al++ar++check)
iExpEval2 env (UIOps (op, exp)) = (a, env', UIOps (op, b))
      where (a, env', b) = iExpEval2 env exp
iExpEval2 env ErrorI = ([Err (VError, NoTh)], env, ErrorI)

-- substite int expr
iSubst :: (IExpr, IExpr) -> IExpr -> IExpr
iSubst (u,v) (Const x) = Const x
iSubst (u,v) (IPARA exp) = IPARA (iSubst (u,v) exp)
iSubst (u,v) (IVar name) = if u==(IVar name) then v else IVar name
iSubst (u,v) (IArc (name, iexp)) = if u==(IArc (name, iexp')) then v else IArc (name,iexp')
      where iexp' = iSubst (u,v) iexp
iSubst (u,v) (BIOp (op, l, r)) = BIOp (op, iSubst (u,v) l, iSubst (u,v) r)
iSubst (u,v) (UIOps (op, e)) = UIOps (op, iSubst (u,v) e)
iSubst (u,v) ErrorI = ErrorI


iFetch :: Env -> IExpr -> Maybe IExpr
iFetch env (Const x) = Nothing
iFetch env (IPARA exp) = iFetch env exp
iFetch env (IVar name) = if isCst then Nothing else Just (IVar name)
      where variable = filterVariable name env
            var = if isJust variable then fromJust variable else error "Unseen variable"
            isCst = isCstVar var
iFetch env (IArc (name, e)) = if isJust e' then e' else Just (IArc (name, e))
      where e' = iFetch env e
iFetch env (BIOp (op, l, r)) = if isJust l' then l' else r'
      where l' = iFetch env l
            r' = iFetch env r
iFetch env (UIOps (op, exp)) = iFetch env exp
iFetch env ErrorI = Nothing



convertToIFetchEvt :: IExpr -> Variable -> VarName -> Event
convertToIFetchEvt (IVar name) loc newName = Econ ("iveval", NoTh, YesLocV (Output, loc), YesIVal (Input, IVar newName))
convertToIFetchEvt (IArc (name, exp)) loc newName = Econ ("iveval", NoTh, YesLocA (Output, loc, exp), YesIVal (Input, IVar newName))

convertToBFetchEvt :: BExpr -> Variable -> VarName -> Event
convertToBFetchEvt (BVar name) loc newName = Econ ("bveval", NoTh, YesLocV (Output, loc), YesIVal (Input, IVar newName))
convertToBFetchEvt (BArc (name, exp)) loc newName = Econ ("bveval", NoTh, YesLocA (Output, loc, exp), YesIVal (Input, IVar newName))


-- eval each expression at most once in an expr
iExpEval1 :: Env -> IExpr -> ([Event], Env, IExpr)
iExpEval1 env exp = if isNothing sub then ([], env, exp) else (e:evts, env2, exp2)
      where sub = iFetch env exp
            name = getIExprVarName $ fromJust sub
            variable = filterVariable name env
            var = if isJust variable then fromJust variable else error "Unseen variable"
            newVarName = getNewNameWEnv env (name++"Val_")
            newVar = createCstVar newVarName
            e = convertToIFetchEvt (fromJust sub) var newVarName
            env1 = addLocalVar newVar env
            exp1 = iSubst (fromJust sub, IVar newVarName) exp 
            (evts, env2, exp2) = iExpEval1 env1 exp1




iLvEval :: Env -> IExpr -> ([Event], Env, IExpr)
iLvEval env (IVar name) = ([], env, IVar name)
iLvEval env (IArc (name, iexp)) = (a, env', IArc (name, b))
      where (a, env', b) = iExpEval env iexp
iLvEval env ErrorI = ([Err (VError, NoTh)], env, ErrorI)
iLvEval env _ = ([], env, ErrorI)

-- this should only be used by signal
idxToFields :: Env -> [IExpr] -> [IExpr]
idxToFields env [] = []
idxToFields env (x:xs) = assert (a == []) b:(idxToFields env xs)
        where (a, env1, b) = iExpEval env x



mainProc :: Env -> Cmd -> (Env, CSPProc)
mainProc env SkipP = (env, SKIP)
mainProc env (Sq (a, b)) = (env2, CSPSEQ (left, right))
      where (env1, left) = mainProc env a
            (env2, right) = mainProc env1 b
mainProc env (SQ []) = (env, SKIP)
mainProc env (SQ (x:xs)) = (env2, CSPSEQ (left, right))
      where (env1, left) = mainProc env x
            (env2, right) = mainProc env1 (SQ xs)
mainProc env (IterC x) = (env2, LET (newName, [], CSPSEQ (subProc, newProc), newProc))
        where (env1, subProc) = mainProc env x
              newName = getNewNameWEnv env1 "tmpProc_"
              env2 = addConstName newName env1
              newProc = APPL (newName, [])
mainProc env (WhileC (con, p)) = (env3, LET (newName, [], resProc, dummyProc))
        where (a, env1, b) = bExpEval env con
              newName = getNewNameWEnv env1 "tmpProc_"
              dummyProc = APPL (newName, [])
              env2 = addConstName newName env1
              (env3, left) = mainProc env2 p
              branch = if (b == ErrorB) then error ("Failed to parse boolean expression: " ++ (show con)) else CSPCON (b, CSPSEQ (left, dummyProc), SKIP)
              resProc = joinEvents a branch
mainProc env (CondOne (con, p)) = (env2, joinEvents a branch)
        where (a, env1, b) = bExpEval env con
              (env2, left) = mainProc env1 p
              branch = if (b == ErrorB) then error ("Failed to parse boolean expression: " ++ (show con)) else CSPCON (b, left, SKIP)
mainProc env (CondTwo (con, p, q)) = (env3, joinEvents a branch)
        where (a, env1, b) = bExpEval env con
              (env2, left) = mainProc env1 p
              (env3, right) = mainProc env2 q
              branch = if (b == ErrorB) then error ("Failed to parse boolean expression: " ++ (show con)) else CSPCON (b, left, right)
mainProc env (LockC (name, idx, bool)) = if isArr then (env1, res) else (env, res)
      where lock' = filterVariable name env
            lock = if isJust lock' then fromJust lock' else error "undefined lock"
            isArr = isArrVar lock
            (a, env1, exp) = iExpEval env idx
            location = if isArr then YesLocA (Output, lock, exp) else YesLocV (Output, lock)
            lockEvt = Econ ("Lock", NoTh, location, YesLVal (Output, bool))
            res = joinEvents (a++[lockEvt]) SKIP
mainProc env (Iassign (l, r)) = (env2, joinEvents (al ++ bl ++ [write]) finalProc)
        where (al, env1, ar) = iLvEval env l
              (bl, env2, br) = iExpEval env1 r
              addIntParam = \x -> if needParamInt x then IPARA x else x
              ar' = if (ar == ErrorI) then error ("Failed to parse integer expression: " ++ (show l)) else addIntParam ar 
              br' = if (br == ErrorI) then error ("Failed to parse integer expression: " ++ (show r)) else addIntParam br 
              variable = filterVariable (getIExprName ar) env2
              loc = if isNothing variable then error ("Undefined varible" ++ show env2) else fromMaybe dummy variable
              chan = "ivwrite"
              location = if isIVar ar then YesLocV (Output, loc) else YesLocA (Output, loc, getIExprArcIdx ar')
            --   write = Econ (chan, [Fcon (Output, LOC location), Fcon (Output, IVAR br')])
              write = Econ (chan, NoTh, location, YesIVal (Output, br'))
              dirtyBool = isDirtyVar loc
              finalProc = if dirtyBool then CONC (write, SKIP) else SKIP
mainProc env (Bassign (l, r)) = (env2, joinEvents (al ++ bl ++ [write]) finalProc)-- follow Iassign above
        where (al, env1, ar) = bLvEval env l
              (bl, env2, br) = bExpEval env1 r
              addBoolParam = \x -> if needParamBool x then BPARA x else x
              ar' = if (ar == ErrorB) then error ("Failed to parse boolean expression: " ++ (show l)) else addBoolParam ar
              br' = if (br == ErrorB) then error ("Failed to parse boolean expression: " ++ (show r)) else addBoolParam br
              variable = filterVariable (getBExprName ar) env2
              loc = if isNothing variable then error "Undefined varible" else fromMaybe dummy variable
              chan = "bvwrite" -- if isBVar ar then "bvwrite" else "bawrite"
              location = if isBVar ar then YesLocV (Output, loc) else YesLocA (Output, loc, getBExprArcIdx ar')
              write = Econ (chan, NoTh, location, YesBVal (Output, br'))
              dirtyBool = isDirtyVar loc
              finalProc = if dirtyBool then CONC (write, SKIP) else SKIP
mainProc env (SigC (name, xs)) = (env, CONC (evt, SKIP))
        where fields = YesLocSig (Output, idxToFields env xs)
              evt = Econ (name, NoTh, fields, NoVal)
mainProc env (ISigC ((name, xs), exp)) = if (b == ErrorI) then error ("Failed to parse integer expression: " ++ (show exp)) else (env1, joinEvents a (joinEvents [write] SKIP))
        where fields = YesLocSig (Output, idxToFields env xs)
              (a, env1, b) = iExpEval env exp
              outputfield = YesIVal (Output, b)
              write = Econ (name, NoTh, fields, outputfield)
mainProc env (AtomicC cmd) = (env1, CSPSEQ (left, right))
        where start = Econ ("start_at", NoTh, NoLoc, NoVal)
              end = Econ ("end_at", NoTh, NoLoc, NoVal)
              (env1, resP) = mainProc env cmd
              left = CONC (start, resP)
              right = CONC (end, SKIP)
mainProc env ErrorC = (env, errorSTOP)



joinEvents :: [Event] -> CSPProc -> CSPProc
joinEvents [] x = x
joinEvents ((DummyEvt (str, exp)):xs) y | str == "check_zero" = CSPCON (CompOp (Eq, IVar exp, Const 0), errorSTOP, joinEvents xs y)
                                        | otherwise = error "Undefined special event"
joinEvents ((Err x):xs) y = CONC ((Err x), STOP) -- stop immediately if an error event is seen, but this should not happen
joinEvents ((Econ (chan, th, loc, val)):xs) y  = checkIdx
                  where isInt = chan!!0 == 'i'
                        isArc = isLocA loc
                        isWrite = (drop 2 chan) == "write"
                        idxVal = getFieldIdx loc
                        idxBool = BVar $ "member(" ++ (show idxVal) ++ "," ++ (itypeStr loc) ++ ")"
                        valBool = BVar $ "member(" ++ (show $ getEventValIExpr val) ++ ","++ (ctypeStr loc) ++ ")"
                        branch = CONC (Econ (chan, th, loc, val), joinEvents xs y)
                        checkVal = if isWrite && isInt then CSPCON(valBool, branch, errorSTOP) else branch
                        checkIdx = if isArc then CSPCON(idxBool, checkVal, errorSTOP) else checkVal
 
                 
getFieldIdx :: Location -> IExpr
getFieldIdx (YesLocV (_, var)) = IVar $ getVariableName var
getFieldIdx (YesLocA (_, var, exp)) = exp
getFieldIdx _ = ErrorI


            
overwriteCSPProcTh :: VarName -> CSPProc -> CSPProc
overwriteCSPProcTh thName (CONC (e, a)) = CONC (resEvent, funcP a)
      where resEvent = overwriteEventTh Output thName e
            funcP = overwriteCSPProcTh thName
overwriteCSPProcTh thName (CSPSEQ (l, r)) = CSPSEQ (funcP l, funcP r)
      where funcP = overwriteCSPProcTh thName
overwriteCSPProcTh thName (LET (pname, ps, l, r)) = LET (pname, ps, funcP l, funcP r)
      where funcP = overwriteCSPProcTh thName
overwriteCSPProcTh thName (CSPCON (exp, l, r)) = CSPCON (exp, funcP l, funcP r)
      where funcP = overwriteCSPProcTh thName
overwriteCSPProcTh thName (EXTCHOICE (l, r)) = EXTCHOICE (funcP l, funcP r)
      where funcP = overwriteCSPProcTh thName
overwriteCSPProcTh thName x = x



mainProcLine :: Env -> AstProc -> CSPProcDef
mainProcLine env (ProcDef (name, ps, cmd)) = CSPDef (name, ps ++ [thName], resProc)
      where (env1, proc1) = mainProc env cmd
            thName = getNewNameWEnv env1 "th"
            resProc = overwriteCSPProcTh thName proc1
            

getProcLocalConst :: AstProc -> [VarName]
getProcLocalConst (ProcDef (name, ps, cmd)) = name:ps


removeSigProcTh :: [VarName] -> CSPProc -> CSPProc
removeSigProcTh names (CONC (e, a)) = CONC (funcE e, funcP a)
      where funcP = removeSigProcTh names
            funcE x = if (not $ isErrEvent x) && (elem (getEvenName x) names) then removeEventTh x else x
removeSigProcTh names (CSPSEQ (l, r)) = CSPSEQ (funcP l, funcP r)
      where funcP = removeSigProcTh names
removeSigProcTh names (LET (pname, ps, l, r)) = LET (pname, ps, funcP l, funcP r)
      where funcP = removeSigProcTh names
removeSigProcTh names (CSPCON (exp, l, r)) = CSPCON (exp, funcP l, funcP r)
      where funcP = removeSigProcTh names
removeSigProcTh names (EXTCHOICE (l, r)) = EXTCHOICE (funcP l, funcP r)
      where funcP = removeSigProcTh names
removeSigProcTh names x = x


getEventSet :: CSPProc -> [Event]
getEventSet SKIP = []
getEventSet STOP = []
getEventSet (CONC (l, r)) = l:(getEventSet r)
getEventSet (CSPSEQ (l, r)) = (getEventSet l)++(getEventSet r)
getEventSet (APPL _) = []
getEventSet (LET (_, _, l, r)) = (getEventSet l)++(getEventSet r)
getEventSet (CSPCON (_, l, r)) = (getEventSet l)++(getEventSet r)
getEventSet (EXTCHOICE (l, r)) = (getEventSet l)++(getEventSet r)


