module RAM where

import Instruction
import Types

data RAM = RAM {
  getInstructions :: [Instruction], 
  getPageFaults :: Integer,
  getReadNum :: Integer,
  getWriteNum :: Integer,
  getInstrCounter :: Integer
} deriving (Eq)  


instance Show RAM where
--  show ram = instrs ++ "\n PageFaults: " ++ pf ++ ", ReadNum: " ++ rn ++ ", WriteNum: " ++ wn ++ ", InstrCounter: " ++ ic
  show ram = "\n PageFaults: " ++ pf ++ ", ReadNum: " ++ rn ++ ", WriteNum: " ++ wn ++ ", InstrCounter: " ++ ic
    where 
        instrs = foldl (\acc x -> acc ++ show x) "" $ getInstructions ram
        pf = show $ getPageFaults ram
        rn = show $ getReadNum ram
        wn = show $ getWriteNum ram
        ic = show $ getInstrCounter ram


startingRAM :: RAM
startingRAM = RAM (replicate 32 Null) 0 0 0 0

incInstrCounter :: RAM -> RAM
incInstrCounter ram = ram {getInstrCounter = (getInstrCounter ram) +1}

incPageFaults :: RAM -> RAM
incPageFaults ram = ram { getPageFaults = (getPageFaults ram)+1} 

incReadNum :: RAM -> RAM
incReadNum ram = ram { getReadNum = (getReadNum ram)+1} 

incWriteNum :: RAM -> RAM
incWriteNum ram = ram { getWriteNum = (getWriteNum ram)+1} 


updateInstrCounter :: RAM -> RAM
updateInstrCounter ram = case (getInstrCounter ram) of  199 -> restartRefBits ram 
                                                        _ -> incInstrCounter ram

restartRefBits :: RAM -> RAM
restartRefBits ram = ram {getInstructions = restartInstrs $ getInstructions ram, getInstrCounter = 0}
        where
            restartInstrs = map (\instr -> if instr == Null then Null else instr {getRefBit = Zero})


--            trace (show $ putInstructionInRAM env (trace (show $ putInstructionInRAM env ram (ft)) $ putInstructionInRAM env ram (ft)) (sd)) $ putInstructionInRAM env (trace (show $ putInstructionInRAM env ram (ft)) $ putInstructionInRAM env ram (ft)) (sd)
