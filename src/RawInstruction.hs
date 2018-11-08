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
  path <- fmap (++ "/" ++ fileName) getCurrentDirectory
  fileRead <- BStr.readFile path
  return $ (map (toRawInstruction . (BStr.split ' ')) . BStr.lines) fileRead

toRawInstruction :: [ByteString] -> RawInstruction
toRawInstruction bstr = RawInstruction (read idProcess) (( (`div` 512) . read) instrDir) (( (`div` 512) . read) memRef) mode
      where
        [idProcess, instrDir, memRef, mode] = map unpack bstr 