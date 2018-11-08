module RAM where

import Instruction

data RAM = RAM {
  getInstructions :: [Instruction], 
  getPageFaults :: Integer,
  getReadNum :: Integer,
  getWriteNum :: Integer,
  getInstrCounter :: Integer
} deriving (Eq, Show)  

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
updateInstrCounter ram = case (getInstrCounter ram) of  200 -> restartRefBits ram 
                                                        _ -> incInstrCounter ram

restartRefBits :: RAM -> RAM
restartRefBits ram = ram {getInstructions = restartInstrs $ getInstructions ram, getInstrCounter = 0}
        where
            restartInstrs = map (\(Instruction pid fn rb db) -> Instruction pid fn Zero db) 

