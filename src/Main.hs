module Main where

import System.Environment
import Data.ByteString.Lazy.Char8 as BStr hiding (putStrLn, map)
import System.Directory

data BoolNum = Zero | One deriving (Show)

data Instruction = Instruction {
                                    getProcessId :: Integer,
                                    getFrameNumber :: Integer,
                                    getReferenceBit :: BoolNum,
                                    getDirtyBit :: BoolNum
                                  } deriving (Show)


readData :: String -> IO [Instruction]
readData fileName = do 
  pathText1 <- fmap (++ ("/src/" ++ fileName)) getCurrentDirectory
  --pathText1 <- return $ "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/src/" ++ fileName
  fileRead <- BStr.readFile pathText1
  return $ (map (toInstruction . (BStr.split ' ')) . BStr.lines) fileRead

toInstruction :: [ByteString] -> Instruction
toInstruction bstr = Instruction (read idProcess) (read frameNumber) One (if dirtyBit == "W" then One else Zero)
      where
        [idProcess, frameNumber, refBit, dirtyBit] = map unpack bstr 



main :: IO ()
main = do
  args <- getArgs
  dataRead <- readData (args!!0)
  print dataRead