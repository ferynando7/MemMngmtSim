module Main where

import RAM
import Instruction
import RawInstruction
import qualified Version1 as V1
import qualified Version2 as V2
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
  results <- case version of  "1" -> return $ V1.loadInstructions ram rawInstructions
                              "2" -> return $ V2.loadInstructions ram rawInstructions
                              other -> error "version no adecuada"
  print results
