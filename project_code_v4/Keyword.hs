module Keyword where


data Keyword = Integer | Boolean | Dirty | ConstK | Min | Max |
               LockK | LockE | UnLockE |
               TrueK | FalseK | Atomic | 
               Skip | While | Iter | Sig | Isig |
               Assert | Always | Never | Null | 
               Do | If | Then | Else | With | MinusSign
               deriving (Eq)

instance Show Keyword where
    show Integer = "Int"
    show Boolean = "Bool"
    show LockK = "Lock"
    show LockE = "LockE"
    show UnLockE = "UnLockE"
    show Dirty = "dirty"
    show ConstK = "const"
    show Min = "min"
    show Max = "max"
    show TrueK = "true"
    show FalseK = "false"
    show Atomic = "atomic"
    show Skip = "SKIP"
    show While = "while"
    show Iter = "iter"
    show Sig = "sig"
    show Isig = "isig"
    show Assert = "assert"
    show Always = "always"
    show Never = "never"
    show Null = "null"
    show Do = "do"
    show If = "if"
    show Then = "then"
    show Else = "else"
    show With = "with"
    show MinusSign = "-"


getKeyword :: String -> Keyword
getKeyword "int" = Integer
getKeyword "bool" = Boolean
getKeyword "lock" = LockK
getKeyword ".lock" = LockE
getKeyword ".unlock" = UnLockE
getKeyword "dirty" = Dirty
getKeyword "const" = ConstK
getKeyword "min" = Min
getKeyword "max" = Max
getKeyword "true" = TrueK
getKeyword "false" = FalseK
getKeyword "atomic" = Atomic
getKeyword "skip" = Skip
getKeyword "while" = While
getKeyword "iter" = Iter
getKeyword "sig" = Sig
getKeyword "isig" = Isig
getKeyword "assert" = Assert
getKeyword "always" = Always
getKeyword "never" = Never
getKeyword "do" = Do
getKeyword "if" = If
getKeyword "then" = Then
getKeyword "else" = Else
getKeyword "with" = With
getKeyword _ = Null