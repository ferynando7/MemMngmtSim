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
  args <- getArgs
  rawInstructions <- readData (args!!0)
  version <- return $ args!!1
--For running from "stack ghci" command enable this
  -- rawInstructions <- readData "/src/data1.txt"
-----------------------------------------------
  ram <- return $ startingRAM
  results <- case version of  "1" -> return $ V1.loadInstructions ram rawInstructions
                              "2" -> return $ V2.loadInstructions ram rawInstructions
                              other -> error "version no adecuada"
  print results
  