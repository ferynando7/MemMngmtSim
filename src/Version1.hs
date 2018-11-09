module Version1 where

import qualified Instruction as I
import RAM
import RawInstruction
import Types
import Debug.Trace



--Esta funcion asume que existe especio en la RAM
addInstruction :: I.Instruction -> RAM -> RAM
addInstruction instr ram = ram {getInstructions = newInstructions}
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

checkDirtyBit :: I.Instruction -> RAM -> RAM
checkDirtyBit instr ram 
        | I.getDirtyBit instr == Zero = ram
        | otherwise = ram {getInstructions = updateInstructions oldInstructions}
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
            
            