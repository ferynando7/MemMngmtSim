module Version2 where

import qualified Instruction as I
import RAM
import RawInstruction
import Types
import Debug.Trace
import Version1 hiding (removeInstruction, replaceInstrucion, putInstructionInRAM, loadInstruction, loadInstructions,checkDirtyNRefBit)
import Data.Maybe
import Data.List

--Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
removeInstruction :: [I.Instruction] -> ([I.Instruction], I.Instruction, Integer)
removeInstruction lst = (fst (setNull lst menor), menor, snd (setNull lst menor))
    where   menor = foldl1 (min) lst
            setNull (x:xs) ins
                | x == ins = (I.Null:xs , 0)
                | otherwise = (x : fst (setNull xs ins) , snd (setNull xs ins) + 1)
                                
replaceInstrucion :: Environment -> I.Instruction -> RAM ->  RAM
replaceInstrucion env instr ram
        | I.getDirtyBit rmInstr == One = dirtyPage
        | otherwise = notDirtyPage
    where 
        dirtyPage = incWriteNum notDirtyPage
        notDirtyPage
            | env == Development = (addInstruction debug instr) ram {getInstructions = nInstrs}
            | otherwise = (addInstruction NoDebug instr) ram {getInstructions = nInstrs}
        (nInstrs, rmInstr, ramPos) = removeInstruction $ getInstructions ram
        debug = Debug (I.getLineNumber instr) ramPos (I.getProcessId rmInstr) (I.getFrameNumber rmInstr) (I.getDirtyBit rmInstr)

checkDirtyNRefBit :: I.Instruction -> RAM -> RAM
checkDirtyNRefBit instr ram
        | I.getDirtyBit instr == Zero = ram {getInstructions = updateInstructions2 oldInstructions}
        | I.getDirtyBit instr == One = ram {getInstructions = updateInstructions2 $ updateInstructions oldInstructions}
    where
        oldInstructions = getInstructions ram
        updateInstructions (x:xs)
            | x == instr = (x {I.getDirtyBit = One}):xs
            | otherwise = x:(updateInstructions xs)
        updateInstructions2 (x:xs)
            | x == instr = (x {I.getLineNumber = (I.getLineNumber instr)}):xs
            | otherwise = x:(updateInstructions2 xs)


putInstructionInRAM :: Environment -> RAM -> I.Instruction ->  RAM
putInstructionInRAM env ram instr
    --Si la instruccion esta en la RAM
    | elem instr (getInstructions nRam) = checkDirtyNRefBit instr ram
        
    --Si la instruccion no esta en la RAM
    --Analizar si hay espacio en la RAM
    | otherwise = case (elem I.Null (getInstructions nRam)) of  False -> replaceInstrucion env instr nRam
                                                                True  -> if env == Development then addInstruction debug instr nRam else addInstruction NoDebug instr nRam

    where
        nRam = (incReadNum . incPageFaults) ram -- Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
        debug = Debug (I.getLineNumber instr) (fromIntegral $ fromJust $ elemIndex I.Null $ (getInstructions nRam)) 0 0 Zero

--Cargo la misma instruccion las dos veces que se necesita
loadInstruction :: Environment -> RAM -> RawInstruction -> RAM
loadInstruction env ram rinstr = putInstructionInRAM env (putInstructionInRAM env ram (ft)) (sd)
        where   ft = I.fromRawInstruction First rinstr 
                sd = I.fromRawInstruction Second rinstr

loadInstructions :: Environment -> RAM -> [RawInstruction] -> RAM
loadInstructions env ram [] = ram
loadInstructions env ram (x:xs) = loadInstructions env (loadInstruction env ram x) xs 