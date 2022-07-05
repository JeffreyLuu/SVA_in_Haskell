module Main where

import Token
import Parser
import Overall
import Parser


import Control.Exception
import System.IO
import System.IO.Error

-- inputFileName = "svaexamples/bakery1/bakery.svl"
inputFileName = "svaexamples/bakery1/bakerysimp.svl"
-- inputFileName = "svaexamples/bakery1/bakerysimp2.svl" -- does not support calling another thread in one thread

-- inputFileName = "svaexamples/bakery2/bakery2.svl" -- longer compilation time for D
-- inputFileName = "svaexamples/bakery2/bakery3.svl" 
-- inputFileName = "svaexamples/bakery2/bakerysimp2.svl" -- longer compilation time for D

-- inputFileName = "svaexamples/dirty/dbakery.svl"
-- inputFileName = "svaexamples/dirty/dbakerysimp.svl"
-- inputFileName = "svaexamples/dirty/ddekker.svl" 
-- inputFileName = "svaexamples/dirty/dknuth.svl" 
-- inputFileName = "svaexamples/dirty/dpeterson.svl"

-- inputFileName = "svaexamples/mutex/dekker.svl" -- more states
-- inputFileName = "svaexamples/mutex/hyman.svl" -- more states
-- inputFileName = "svaexamples/mutex/knuth.svl" -- fewer states
-- inputFileName = "svaexamples/mutex/knuthtest.svl" -- does not support calling another thread in one thread
-- inputFileName = "svaexamples/mutex/peterson.svl"
-- inputFileName = "svaexamples/mutex/petersonN.svl" -- too long to compile for both

-- slightly more states/transitions for all cases (5% more states)
-- inputFileName = "svaexamples/simpson/simpson.svl"
-- inputFileName = "svaexamples/simpson/simpson2.svl"
-- inputFileName = "svaexamples/simpson/simpson3.svl"
-- inputFileName = "svaexamples/simpson/simpson4.svl"
-- inputFileName = "svaexamples/simpson/simpson5.svl"

-- inputFileName = "svaexamples/svdphils/sphils2.svl" -- kind of cheating, use only much fewer states


main :: IO()
main = compile inputFileName

handler e = 
  if isIllegalOperation e 
  then putStr "I/O error (maybe file not found)\nGoodbye\n" 
  else putStr ("Error: "++show e++"\nGoodbye\n")

compile :: String -> IO()
compile s = catch (compile0 s) handler

compile0 :: String -> IO()
compile0 filename = 
  do contents   <- readContent filename
     compile2 filename contents

readContent :: String -> IO String
readContent filename = 
    do {fromHandle <- openFile filename ReadMode;
        --putStr ("Parsing...\n");
        hGetContents fromHandle}

compile2 :: String -> String -> IO()
compile2 filename conts =
    do { --putStr "Type checking...\n";
        testing lines}
        --printASTs asts}
     where  lines = parse conts

-- compile3 :: String -> String -> IO()
-- compile3 filename conts =
--     do {putStr "Type checking...\n";
--         print' asts}
--      where asts = map tokenize $ parse conts


print' [] = putStr "\n"
print' (x:xs) = do {print $ tokenize x; putStr "\n"; print' xs}

print2 [] = putStr "\n"
print2 (x:xs) = do {putStr x; putStr "\n"; print2 xs}


testing xs = do {
    putStr "include \"Helper2.csp\"\n\n";
    -- print' xs;
    print overall;
}
    where overall = rawCompileAll xs
