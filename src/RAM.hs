module RAM where

import Instruction

data RAM = RAM {
  getInstructions :: [Instruction], 
  getPageFaults :: Integer,
  getDiskReferences :: Integer,
  getInstrCounter :: Integer
} deriving (Eq, Show)  

isInstructionInRam :: RAM -> Instruction -> Bool
isInstructionInRam ram@(RAM [] _ _ _) _ = False
isInstructionInRam ram@(RAM (x:xs) _ _ _) instr = x == instr || isInstructionInRam (ram {getInstructions = xs}) instr 

addInstruction :: Instruction -> RAM -> RAM
addInstruction instr (RAM instrs faults refs counter) = RAM (instr:instrs) faults refs counter

incInstrCounter :: RAM -> RAM
incInstrCounter ram = ram {getInstrCounter = (getInstrCounter ram) +1}

incPageFaults :: RAM -> RAM
incPageFaults ram = ram { getPageFaults = (getPageFaults ram)+1} 

incDisksRefs :: RAM -> RAM
incDisksRefs ram = ram { getDiskReferences = (getDiskReferences ram)+1} 

replaceInstrucionV1 :: Instruction -> RAM ->  RAM
replaceInstrucionV1 instr ram@(RAM instrs faults refs counter)
      | bool == One = (incDisksRefs . (addInstruction instr)) (RAM nInstrs faults refs counter)
      | otherwise = (addInstruction instr) (RAM nInstrs faults refs counter)
  where 
    (nInstrs, bool) = removeInstructionV1 instrs

updateInstrCounter :: RAM -> RAM
updateInstrCounter ram = case (getInstrCounter ram) of  200 -> restartRefBits ram 
                                                        _ -> incInstrCounter ram



restartRefBits :: RAM -> RAM
restartRefBits (RAM instrs pageFaults refBits counter) = RAM (restartInstrs instrs) pageFaults refBits 0
        where
            restartInstrs = map (\(Instruction pid fn rb db) -> Instruction pid fn Zero db) 

loadInstructionV1 :: RAM -> Instruction -> RAM
loadInstructionV1 ram instr
    --Si la instruccion esta en la RAM
    | isInstructionInRam nRam instr = nRam
        
    --Si la instruccion no esta en la RAM
    | otherwise = let nnRam = (incDisksRefs . incPageFaults) nRam --Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
            in
                --Analizar si hay espacio en la RAM
                case (length (getInstructions nnRam)) of    32 -> replaceInstrucionV1 instr nnRam
                                                            _  -> addInstruction instr nnRam

    where
    nRam = updateInstrCounter ram

loadInstructionsV1 :: RAM -> [Instruction] -> RAM
loadInstructionsV1 ram [] = ram
loadInstructionsV1 ram (x:xs) = loadInstructionsV1 (loadInstructionV1 ram x) xs 
