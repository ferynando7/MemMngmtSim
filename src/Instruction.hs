module Instruction where

import qualified RawInstruction as RI
import Types

data Instruction = Null | Instruction {
    getLineNumber :: Integer,
    getProcessId :: Integer,
    getFrameNumber :: Integer,
    getRefBit :: BoolNum,
    getDirtyBit :: BoolNum
}

instance Show Instruction where
    show Null = "\nNull"
    show instr = "\nln = " ++ ln ++ ", pid = " ++ pid ++ ", fn = " ++ fn ++ ", rb = " ++ rb ++ ", db = " ++ db
        where
            ln = show $ getLineNumber instr
            pid = show $ getProcessId instr
            fn = show $ getFrameNumber instr
            rb = show $ getRefBit instr
            db = show $getDirtyBit instr
instance Eq Instruction where
    Null == Null = True
    _ == Null = False
    Null == _ = False
    inst1 == inst2
        | (getProcessId inst1, getFrameNumber inst1) == (getProcessId inst2, getFrameNumber inst2) = True
        | otherwise = False 
  
instance Ord Instruction where
    compare inst1 inst2 
                | (getLineNumber inst1) < (getLineNumber inst2) = LT
                | (getLineNumber inst1) > (getLineNumber inst2) = GT
                | otherwise = LT
                    -- case (getDirtyBit inst1, getDirtyBit inst2) of  (One, Zero) -> GT
                    --                                                 _  -> LT



fromRawInstruction :: Opening -> RI.RawInstruction ->  Instruction
fromRawInstruction opn (RI.RawInstruction ln pid instrDir memRef mode)
    | opn == First = Instruction ln pid instrDir One Zero
    | otherwise = Instruction ln pid memRef One (if mode == "W" then One else (if mode == "R" then Zero else NAN))