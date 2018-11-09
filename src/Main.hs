module Main where

import RAM
import Instruction
import RawInstruction
import qualified Version1 as V1
import System.Environment

main :: IO ()
main = do
-----------------------------------------------
--For building executable file enable this
  --args <- getArgs
  --rawInstructions <- readData (args!!0)
  --version <- (args!!1)
  --debug <- if (length args == 3) then 1 else 0
--For running from "stack ghci" command enable this
  putStr "File: "
  file <- getLine
  rawInstructions <- readData $ "/src/" ++ file
  putStr "Version: "
  version <- getLine 
  putStr "Debug: "
  version <- getLine
  -----------------------------------------------
  ram <- return $ startingRAM
  results <- return $ V1.loadInstructions ram rawInstructions
  print results