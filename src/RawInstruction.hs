module RawInstruction where

import Data.ByteString.Lazy.Char8 as BStr (ByteString, lines, readFile, split, unpack)
import System.Directory


data RawInstruction = RawInstruction {
  getProcessId :: Integer,
  getInstructionDir :: Integer,
  getMemReference :: Integer,
  getMode :: String
} deriving (Show)

readData :: String -> IO [RawInstruction]
readData fileName = do 
  pathText1 <- fmap (++ ("/src/" ++ fileName)) getCurrentDirectory
  --pathText1 <- return $ "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/src/" ++ fileName
  fileRead <- BStr.readFile pathText1
  return $ (map (toRawInstruction . (BStr.split ' ')) . BStr.lines) fileRead

toRawInstruction :: [ByteString] -> RawInstruction
toRawInstruction bstr = RawInstruction (read idProcess) (( (`div` 512) . read) instrDir) (( (`div` 512) . read) memRef) mode
      where
        [idProcess, instrDir, memRef, mode] = map unpack bstr 