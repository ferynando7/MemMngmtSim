module Instruction where

import qualified RawInstruction as RI

data BoolNum = Zero | One deriving (Eq, Show)
data Opening = First | Second deriving (Eq)

data Instruction = Null | Instruction {
  getProcessId :: Integer,
  getFrameNumber :: Integer,
  getRefBit :: BoolNum,
  getDirtyBit :: BoolNum
} deriving (Show)


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



fromRawInstruction :: RI.RawInstruction -> Opening -> Instruction
fromRawInstruction (RI.RawInstruction pid instrDir memRef mode) opn
    | opn == First = Instruction pid instrDir One Zero
    | otherwise = Instruction pid memRef One (if mode == "W" then One else Zero)