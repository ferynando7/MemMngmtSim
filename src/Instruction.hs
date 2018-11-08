module Instruction where

import qualified RawInstruction as RI

data BoolNum = Zero | One deriving (Eq, Show)
data Opening = First | Second deriving (Eq)

data Instruction = Null | Instruction {
  getProcessId :: Integer,
  getFrameNumber :: Integer,
  getRefBit :: BoolNum,
  getDirtyBit :: BoolNum
}

instance Show Instruction where
    show Null = "\nNull"
    show instr = "\npid = " ++ pid ++ ", fn = " ++ fn ++ ", rb = " ++ rb ++ ", db = " ++ db
        where
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
                | (getFrameNumber inst1) < (getFrameNumber inst2) = LT
                | (getFrameNumber inst1) > (getFrameNumber inst2) = GT
                | otherwise = EQ



fromRawInstruction :: Opening -> RI.RawInstruction ->  Instruction
fromRawInstruction opn (RI.RawInstruction pid instrDir memRef mode)
    | opn == First = Instruction pid instrDir One Zero
    | otherwise = Instruction pid memRef One (if mode == "W" then One else Zero)