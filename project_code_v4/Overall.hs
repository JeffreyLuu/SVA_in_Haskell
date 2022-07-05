module Overall where

import Helper
import Token
import Assertion
import Signal
import Variable
import ProgramStruct

data Overall = Overall ([Variable], [Variable], [ProcWLocal], [Assertion], [SigDeclar], [Program]) deriving (Eq)


