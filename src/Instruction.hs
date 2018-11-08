module Instruction where

import qualified RawInstruction as RI

data BoolNum = Zero | One deriving (Eq, Show)
data Opening = First | Second deriving (Eq)

data Instruction = Instruction {
  getProcessId :: Integer,
  getFrameNumber :: Integer,
  getRefBit :: BoolNum,
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

--Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
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

--Devuelve la instruccion que se elimino, ademas su dirty bit
getMinimalInstr :: [Instruction] -> (Instruction, BoolNum)
getMinimalInstr lst@(x:xs) =  (eliminated , getDirtyBit eliminated)
        where 
            eliminated = (foldr1 (\x acc -> if x < acc then x else acc)) lst

fromRawInstruction :: RI.RawInstruction -> Opening -> Instruction
fromRawInstruction (RI.RawInstruction pid instrDir memRef mode) opn
    | opn == First = Instruction pid instrDir One Zero
    | otherwise = Instruction pid memRef One (if mode == "W" then One else Zero)