module Main where

import System.Environment
import Data.ByteString.Lazy.Char8 as BStr (ByteString, lines, readFile, split, unpack)
import System.Directory

data BoolNum = Zero | One deriving (Eq, Show)

data Instruction = Instruction {
  getProcessId :: Integer,
  getFrameNumber :: Integer,
  getReferenceBit :: BoolNum,
  getDirtyBit :: BoolNum
} deriving (Show)

instance Eq Instruction where
  inst1 == inst2
                | (getProcessId inst1, getFrameNumber inst1) == (getProcessId inst2, getFrameNumber inst2) = True
                | otherwise = False 

instance Ord Instruction where
  compare inst1 inst2 
                | (getFrameNumber inst1) < (getFrameNumber inst2) = LT
                | (getFrameNumber inst1) > (getFrameNumber inst2) = GT
                | otherwise = EQ



data RAM = RAM {
  getInstructions :: [Instruction], 
  getPageFaults :: Integer,
  getDiskReferences :: Integer,
  getInstrCounter :: Integer
} deriving (Eq, Show)  

isInstructionInRam :: [Instruction] -> Instruction -> Bool
isInstructionInRam [] _ = False
isInstructionInRam (x:xs) instr = x == instr || isInstructionInRam xs instr 

addInstruction :: Instruction -> RAM -> RAM
addInstruction instr (RAM instrs faults refs counter) = RAM (instr:instrs) faults refs counter

incInstrCounter :: RAM -> RAM
incInstrCounter (RAM instrs faults refs counter) = RAM instrs faults refs (counter+1) 

incPageFaults :: RAM -> RAM
incPageFaults (RAM instrs faults refs counter) = RAM instrs (faults+1) refs counter 

incDisksRefs :: RAM -> RAM
incDisksRefs (RAM instrs faults refs counter) = RAM instrs faults (refs+1) counter 

replaceInstrucionV1 :: Instruction -> RAM ->  RAM
replaceInstrucionV1 instr ram@(RAM instrs faults refs counter)
      | bool == One = (incDisksRefs . (addInstruction instr)) (RAM nInstrs faults refs counter)
      | otherwise = (addInstruction instr) (RAM nInstrs faults refs counter)
  where 
    (nInstrs, bool) = removeInstructionV1 instrs

updateInstrCounter :: RAM -> RAM
updateInstrCounter ram = case (getInstrCounter ram) of  200 -> restartRefBits ram 
                                                        _ -> incInstrCounter ram

removeInstructionV1 :: [Instruction] -> ([Instruction], BoolNum)
removeInstructionV1 lst
  | not $ null zz = ( filter (/= (fst $ getMinimalInstr zz)) lst , snd $ getMinimalInstr zz)
  | not $ null zo = ( filter (/= (fst $ getMinimalInstr zo)) lst , snd $ getMinimalInstr zo)
  | not $ null oz = ( filter (/= (fst $ getMinimalInstr oz)) lst , snd $ getMinimalInstr oz)
  | not $ null oo = ( filter (/= (fst $ getMinimalInstr oo)) lst , snd $ getMinimalInstr oo)
  | otherwise = error "removeInstructionError" 
      where 
        zz = filter (\(Instruction _ _ ref dirty) -> if ref==Zero && dirty==Zero then True else False) lst
        zo = filter (\(Instruction _ _ ref dirty) -> if ref==Zero && dirty==One then True else False) lst
        oz = filter (\(Instruction _ _ ref dirty) -> if ref==One && dirty==Zero then True else False) lst
        oo = filter (\(Instruction _ _ ref dirty) -> if ref==One && dirty==One then True else False) lst

getMinimalInstr :: [Instruction] -> (Instruction, BoolNum)
getMinimalInstr lst@(x:xs) =  (eliminated , getDirtyBit eliminated)
        where 
          eliminated = (foldr1 (\x acc -> if x < acc then x else acc)) lst

restartRefBits :: RAM -> RAM
restartRefBits (RAM instrs pageFaults refBits counter) = RAM (restartInstrs instrs) pageFaults refBits 0
        where
          restartInstrs = map (\(Instruction pid fn rb db) -> Instruction pid fn Zero db) 

loadInstructionV1 :: RAM -> Instruction -> RAM
loadInstructionV1 ram instr
    --Si la instruccion esta en la RAM
    | isInstructionInRam (getInstructions nRam) instr = nRam
      
    --Si la instruccion no esta en la RAM
    | otherwise = let nnRam = (incDisksRefs . incPageFaults) nRam --Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
          in
              --Analizar si hay espacio en la RAM
              case (length (getInstructions nnRam)) of  32 -> replaceInstrucionV1 instr nnRam
                                                        _  -> addInstruction instr nnRam

  where
    nRam = updateInstrCounter ram

loadInstructionsV1 :: RAM -> [Instruction] -> RAM
loadInstructionsV1 ram [] = ram
loadInstructionsV1 ram (x:xs) = loadInstructionsV1 (loadInstructionV1 ram x) xs 


readData :: String -> IO [Instruction]
readData fileName = do 
  pathText1 <- fmap (++ ("/src/" ++ fileName)) getCurrentDirectory
  --pathText1 <- return $ "/home/fz/Desktop/8vo ciclo/OS/Tasks/Simulador/src/" ++ fileName
  fileRead <- BStr.readFile pathText1
  return $ (map (toInstruction . (BStr.split ' ')) . BStr.lines) fileRead

toInstruction :: [ByteString] -> Instruction
toInstruction bstr = Instruction (read idProcess) (( (`div` 512) . read) frameNumber) One (if dirtyBit == "W" then One else Zero)
      where
        [idProcess, frameNumber, refBit, dirtyBit] = map unpack bstr 



main :: IO ()
main = do
--  args <- getArgs
--  dataRead <- readData (args!!0)
--  print dataRead

  instructions <- readData "data1.txt"
  ram <- return $ RAM instructions 0 0 0
  results <- return $ loadInstructionV1 ram (instructions!!0)
  print results
  