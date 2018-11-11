module Version1 where

import qualified Instruction as I
import RAM
import RawInstruction
import Types
import Debug.Trace
import Data.Maybe
import Data.List




--Esta funcion asume que existe especio en la RAM
addInstruction :: Debug -> I.Instruction -> RAM -> RAM
addInstruction debug instr ram
    | debug == NoDebug = ram {getInstructions = newInstructions}   
    | otherwise = trace (show debug) $ ram {getInstructions = newInstructions}   
    where 
        newInstructions = insertInstruction $ getInstructions ram
        insertInstruction (x:xs)
            | x == I.Null = instr:xs
            | otherwise = x:(insertInstruction xs)

--Devuelve la lista de instrucciones y ademas la instrucción que se eliminó
removeInstruction :: [I.Instruction] -> ([I.Instruction], I.Instruction, Integer)
removeInstruction lst
    | exist lst Zero Zero = setNull lst Zero Zero 0
    | exist lst Zero One = setNull lst Zero One 0
    | exist lst One Zero = setNull lst One Zero 0
    | exist lst One One = setNull lst One One 0
    | otherwise = error "removeInstructionError" 
        where
            exist [] _ _ = False
            exist (x:xs) refBit dirtyBit
                | (I.getRefBit x, I.getDirtyBit x) == (refBit, dirtyBit) = True
                | otherwise = False || exist xs refBit dirtyBit
            setNull (x:xs) refBit dirtyBit cont
                | (I.getRefBit x, I.getDirtyBit x) == (refBit, dirtyBit) = (,,) (I.Null:xs) x cont
                | otherwise = 
                    let newTuple = setNull xs refBit dirtyBit (cont + 1)
                        frst = (\(x,_,_) -> x)
                        scnd = (\(_,x,_) -> x)
                        thrd = (\(_,_,x) -> x)
                    in (,,) (x:(frst newTuple)) (scnd newTuple) (thrd newTuple)


replaceInstrucion :: Environment -> I.Instruction -> RAM ->  RAM
replaceInstrucion env instr ram
        | I.getDirtyBit rmInstr == One = dirtyPage
        | I.getDirtyBit rmInstr == Zero = notDirtyPage
    where 
        dirtyPage = incWriteNum notDirtyPage
        notDirtyPage
            | env == Development = (addInstruction debug instr) ram {getInstructions = nInstrs}
            | otherwise = (addInstruction NoDebug instr) ram {getInstructions = nInstrs}
        (nInstrs, rmInstr, ramPos) = removeInstruction $ getInstructions ram
        debug = Debug (I.getLineNumber instr) ramPos (I.getProcessId rmInstr) (I.getFrameNumber rmInstr) (I.getDirtyBit rmInstr)

checkDirtyBit :: I.Instruction -> RAM -> RAM
checkDirtyBit instr ram 
        | I.getDirtyBit instr == Zero = ram
        | I.getDirtyBit instr == One = ram {getInstructions = updateInstructions oldInstructions}
            where 
                oldInstructions = getInstructions ram
                updateInstructions (x:xs)
                        | x == instr = (x {I.getDirtyBit = One}):xs 
                        | otherwise = x:(updateInstructions xs)


putInstructionInRAM :: Environment -> RAM -> I.Instruction ->  RAM
putInstructionInRAM env ram instr
    --Si la instruccion esta en la RAM
    | elem instr (getInstructions nRam) = checkDirtyBit instr nRam
        
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
            
            