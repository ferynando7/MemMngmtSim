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
--For running from "stack ghci" command enable this
  rawInstructions <- readData "/src/data1.txt"
-----------------------------------------------
  ram <- return $ startingRAM
  results <- return $ V1.loadInstructions ram rawInstructions
  print results
  