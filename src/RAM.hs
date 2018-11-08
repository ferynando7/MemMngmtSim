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

incInstrCounter :: RAM -> RAM
incInstrCounter ram = ram {getInstrCounter = (getInstrCounter ram) +1}

incPageFaults :: RAM -> RAM
incPageFaults ram = ram { getPageFaults = (getPageFaults ram)+1} 

incDisksRefs :: RAM -> RAM
incDisksRefs ram = ram { getDiskReferences = (getDiskReferences ram)+1} 

updateInstrCounter :: RAM -> RAM
updateInstrCounter ram = case (getInstrCounter ram) of  200 -> restartRefBits ram 
                                                        _ -> incInstrCounter ram
                                                        
restartRefBits :: RAM -> RAM
restartRefBits (RAM instrs pageFaults refBits counter) = RAM (restartInstrs instrs) pageFaults refBits 0
        where
            restartInstrs = map (\(Instruction pid fn rb db) -> Instruction pid fn Zero db) 

