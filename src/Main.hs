module Main where

import RAM
import Instruction
import RawInstruction

import System.Environment




main :: IO ()
main = do
--  args <- getArgs
--  dataRead <- readData (args!!0)
--  print dataRead

  instructions <- readData "data1.txt"
  ram <- return $ RAM [] 0 0 0
  results <- return $ loadInstructionV1 ram (fromRawInstruction (instructions!!0) Second)
  print results
  