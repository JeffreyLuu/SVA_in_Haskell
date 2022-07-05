module Signal where

import Token
import IExpr
import Keyword
import Helper

type SignalCmd = (VarName, [IExpr]) -- name of the signal channel

printSignal :: SignalCmd -> String
printSignal (name, es) = if length es == 0 then name else name ++ "." ++ expStr
      where expStr = join "." $ map show es


getSignalChannel :: [Tokentype] -> SignalCmd
getSignalChannel xs = (channel, values)
          where channel = getTokenValue $ head xs
                values = map getIExpr $ split (tail xs) (==(Special "."))

keepPrefSig :: SignalCmd -> SignalCmd
keepPrefSig (name, iexpr) = (name, func iexpr)
    where func [] = []
          func (x:xs) = if isConstIExpr x then x:(func xs) else []



----------------------------------------

data SignalType = SIG | ISIG deriving (Show, Eq)
data SigDeclar = Signal (SignalType, VarName, Int) deriving (Eq)


instance Show SigDeclar where
    show (Signal (tp, name, n)) = "channel " ++ name ++ end
        where str = join "." $ map (\x -> "AllInts") [1..n]
              end = if str == "" then "" else " : " ++ str


isSignalLine :: String -> Bool
isSignalLine s = (xs /= []) && (elem (head xs) [Keyword Sig, Keyword Isig])
        where xs = tokenize s

getSigDeclar :: [String] -> [SigDeclar]
getSigDeclar [] = []
getSigDeclar (x:xs) | isSignalLine x = (declareSignals $ tokenize x) ++ remain
                    | otherwise = remain
        where remain = getSigDeclar xs



mapToSignalType :: Tokentype -> SignalType
mapToSignalType (Keyword Sig) = SIG
mapToSignalType (Keyword Isig) = ISIG
mapToSignalType _ = error "Invalid signal type for signal line"


getIdxFields :: [Tokentype] -> Int
getIdxFields xs = length fss
    where fss = split xs (==(Special "."))


declareSingleSig :: [Tokentype] -> SigDeclar
declareSingleSig xs = Signal (SIG, name, fs)
    where (l, r) = topLvlParenthesisBreak (==(Special ":")) xs
          name = getSingleVarName l
          fs = getIdxFields  r

declareSingleISig :: [Tokentype] -> SigDeclar
declareSingleISig xs = Signal (ISIG, name, fs+1)
    where (l, r) = topLvlParenthesisBreak (==(Special ":")) xs
          name = getSingleVarName l
          fs = getIdxFields  r

declareSignals :: [Tokentype] -> [SigDeclar]
declareSignals xs = if tp == SIG then signals else isignals
    where line = if (last xs) == (Special ";") then init xs else xs
          tp = mapToSignalType $ head line
          signals = map declareSingleSig $ split (tail line) (==(Special ","))
          isignals = map declareSingleISig $ split (tail line) (==(Special ","))

getSignalType :: SigDeclar -> SignalType
getSignalType (Signal (tp, _, _)) = tp

getSignalName :: SigDeclar -> VarName
getSignalName (Signal (_, name, _)) = name


