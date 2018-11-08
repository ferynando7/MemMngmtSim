module Main where

import RAM
import Instruction
import RawInstruction
import qualified Version1 as V1
import System.Environment




main :: IO ()
main = do
--  args <- getArgs
--  dataRead <- readData (args!!0)
--  print dataRead

  instructions <- readData "data1.txt"
  ram <- return $ RAM [] 0 0 0
  results <- return $ V1.loadInstruction ram (fromRawInstruction (instructions!!0) Second)
  print results
  