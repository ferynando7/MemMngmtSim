module Version2 where

import qualified Instruction as I
import RAM
import RawInstruction
import Types
import Debug.Trace
import Version1 hiding (removeInstruction, replaceInstrucion, putInstructionInRAM, loadInstruction, loadInstructions)

--Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
removeInstruction :: [I.Instruction] -> ([I.Instruction], I.Instruction, Integer)
removeInstruction lst = (fst (setNull lst menor), menor, snd (setNull lst menor))
    where   menor = foldl1 (\men elem -> if elem < men then elem else men) lst
            setNull (x:xs) ins
                | x == ins = (I.Null:xs , 0)
                | otherwise = (x : fst (setNull xs ins) , snd (setNull xs ins) + 1)
                                
replaceInstrucion :: Environment -> I.Instruction -> RAM ->  RAM
replaceInstrucion env instr ram
        | I.getDirtyBit rmInstr == One = 
            case env of Development -> trace (unwords debugString) dirtyPage
                        Production -> dirtyPage
        | otherwise = 
            case env of Development -> trace (unwords debugString) notDirtyPage
                        Production -> notDirtyPage
    where 
        dirtyPage = incWriteNum notDirtyPage
        notDirtyPage = (addInstruction instr) ram {getInstructions = nInstrs}
        (nInstrs, rmInstr, ramPos) = removeInstruction $ getInstructions ram
        debugString = [show $ I.getLineNumber instr, show $ ramPos, show $ I.getProcessId rmInstr, show $ I.getFrameNumber rmInstr, show $ I.getDirtyBit rmInstr]



putInstructionInRAM :: Environment -> RAM -> I.Instruction ->  RAM
putInstructionInRAM env ram instr
    --Si la instruccion esta en la RAM
    | elem instr (getInstructions nRam) = checkDirtyBit instr nRam
        
    --Si la instruccion no esta en la RAM
    | otherwise = let nnRam = (incReadNum . incPageFaults) nRam --Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
            in
                --Analizar si hay espacio en la RAM
                case (elem I.Null (getInstructions nnRam)) of   False -> replaceInstrucion env instr nnRam
                                                                True  -> addInstruction instr nnRam

    where
        nRam = updateInstrCounter ram

--Cargo la misma instruccion las dos veces que se necesita
loadInstruction :: Environment -> RAM -> RawInstruction -> RAM
loadInstruction env ram rinstr = putInstructionInRAM env (putInstructionInRAM env ram (ft)) (sd)
        where   ft = I.fromRawInstruction First rinstr 
                sd = I.fromRawInstruction Second rinstr

loadInstructions :: Environment -> RAM -> [RawInstruction] -> RAM
loadInstructions env ram [] = ram
loadInstructions env ram (x:xs) = loadInstructions env (loadInstruction env ram x) xs 