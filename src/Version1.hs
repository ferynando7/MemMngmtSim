module Version1 where

import Instruction
import RAM
import RawInstruction




--Esta funcion asume que existe especio en la RAM
addInstruction :: Instruction -> RAM -> RAM
addInstruction instr ram = ram {getInstructions = newInstructions}
    where 
        newInstructions = insertInstruction $ getInstructions ram
        insertInstruction (x:xs)
            | x == Null = instr:xs
            | otherwise = x:(insertInstruction xs)

--Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
removeInstruction :: [Instruction] -> ([Instruction], BoolNum)
removeInstruction lst
    | exist lst Zero Zero = setNull lst Zero Zero
    | exist lst Zero One = setNull lst Zero One
    | exist lst One Zero = setNull lst One Zero
    | exist lst One One = setNull lst One One
    | otherwise = error "removeInstructionError" 
        where
            exist [] _ _ = False
            exist (x:xs) refBit dirtyBit
                -- | x == Null = False
                | (getRefBit x, getDirtyBit x) == (refBit, dirtyBit) = True
                | otherwise = False || exist xs refBit dirtyBit
            setNull (x:xs) refBit dirtyBit
                | (getRefBit x, getDirtyBit x) == (refBit, dirtyBit) = (Null:xs, getDirtyBit x)
                | otherwise = let newTuple = setNull xs refBit dirtyBit
                                in (x:(fst newTuple), snd newTuple)


replaceInstrucion :: Instruction -> RAM ->  RAM
replaceInstrucion instr ram
        | bool == One = (incWriteNum . (addInstruction instr)) ram {getInstructions = nInstrs}
        | otherwise = (addInstruction instr) ram {getInstructions = nInstrs}
    where 
    (nInstrs, bool) = removeInstruction $ getInstructions ram


checkDirtyBit :: Instruction -> RAM -> RAM
checkDirtyBit instr ram 
        | getDirtyBit instr == Zero = ram
        | otherwise = ram {getInstructions = updateInstructions oldInstructions}
            where 
                oldInstructions = getInstructions ram
                updateInstructions (x:xs)
                        | x == instr = (x {getDirtyBit = One}):xs 
                        | otherwise = x:(updateInstructions xs)


putInstructionInRAM :: RAM -> Instruction ->  RAM
putInstructionInRAM ram instr
    --Si la instruccion esta en la RAM
    | elem instr (getInstructions nRam) = checkDirtyBit instr nRam
        
    --Si la instruccion no esta en la RAM
    | otherwise = let nnRam = (incReadNum . incPageFaults) nRam --Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
            in
                --Analizar si hay espacio en la RAM
                case (elem Null (getInstructions nnRam)) of False -> replaceInstrucion instr nnRam
                                                            True  -> addInstruction instr nnRam

    where
        nRam = updateInstrCounter ram

--Cargo la misma instruccion las dos veces que se necesita
loadInstruction :: RAM -> RawInstruction -> RAM
loadInstruction ram rinstr = putInstructionInRAM (putInstructionInRAM ram (ft)) (sd)
        where   ft = fromRawInstruction First rinstr 
                sd = fromRawInstruction Second rinstr

loadInstructions :: RAM -> [RawInstruction] -> RAM
loadInstructions ram [] = ram
loadInstructions ram (x:xs) = loadInstructions (loadInstruction ram x) xs 
            
            