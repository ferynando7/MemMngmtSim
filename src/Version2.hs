module Version2 where

import qualified Instruction as I
import RAM
import RawInstruction
import Types
import Debug.Trace
import Version1 hiding (removeInstruction, replaceInstrucion, putInstructionInRAM, loadInstruction, loadInstructions)
import Data.Maybe
import Data.List

--Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
removeInstruction :: [I.Instruction] -> ([I.Instruction], I.Instruction, Integer)
removeInstruction lst = (fst (setNull lst menor), menor, snd (setNull lst menor))
    where   menor = foldl1 (\men elem -> if elem < men then elem else men) lst
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


putInstructionInRAM :: Environment -> RAM -> I.Instruction ->  RAM
putInstructionInRAM env ram instr
    --Si la instruccion esta en la RAM
    | elem instr (getInstructions nRam) = nRam
        
    --Si la instruccion no esta en la RAM
    --Analizar si hay espacio en la RAM
    | otherwise = case (elem I.Null (getInstructions nnRam)) of False -> replaceInstrucion env instr nnRam
                                                                True  -> if env == Development then addInstruction debug instr nnRam else addInstruction NoDebug instr nnRam

    where
        nnRam = (incReadNum . incPageFaults) nRam -- Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
        nRam = updateInstrCounter ram
        debug = Debug (I.getLineNumber instr) (fromIntegral $ fromJust $ elemIndex I.Null $ (getInstructions nnRam)) 0 0 Zero

                
--Cargo la misma instruccion las dos veces que se necesita
loadInstruction :: Environment -> RAM -> RawInstruction -> RAM
loadInstruction env ram rinstr = putInstructionInRAM env (putInstructionInRAM env ram (ft)) (sd)
        where   ft = I.fromRawInstruction First rinstr 
                sd = I.fromRawInstruction Second rinstr

loadInstructions :: Environment -> RAM -> [RawInstruction] -> RAM
loadInstructions env ram [] = ram
loadInstructions env ram (x:xs) = loadInstructions env (loadInstruction env ram x) xs 