module Main where

import RAM
import Instruction
import RawInstruction
import qualified Version1 as V1
import qualified Version2 as V2
import System.Environment
import Types
import qualified System.IO.Strict as S




main :: IO ()
main = do
-----------------------------------------------
--For building executable file enable this
  -- args <- getArgs
  -- rawInstructions <- readData (args!!0)
  -- version <- return $ (args!!1)
  -- debug <- if (length args == 3) then return $ Development else return $ Production
--For running from "stack ghci" command enable this
  putStr "File: "
  file <- getLine
  rawInstructions <- readData $ "/src/" ++ file
  putStr "Version: "
  version <- getLine 
  putStr "Debug: "
  debug <- fmap (\lst -> if length lst == 0 then Production else Development) getLine
  -----------------------------------------------
  ram <- return $ startingRAM
  results <- case version of  "1" -> return $ V1.loadInstructions debug ram rawInstructions
                              "2" -> return $ V2.loadInstructions debug ram rawInstructions
                              other -> error "version no adecuada"
  print $! results
-- fmap (take 200) $