module SignalLine where

import Signal
import Token
import Helper

printSingalSetLine :: [SigDeclar] -> String
printSingalSetLine xs = "Signals = {| " ++ names ++ " |}"
    where names = join "," $ map getSignalName xs

printSignalLines :: [SigDeclar] -> String
printSignalLines xs = join "\n" [indivLines, setLine]
    where setLine = printSingalSetLine xs
          indivLines = join "\n" $ map show xs

