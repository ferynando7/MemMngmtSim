module Version2 where

    import Instruction 
    import RAM
    import RawInstruction
    import Version1 hiding (removeInstruction, replaceInstrucion, putInstructionInRAM, loadInstruction, loadInstructions)
    
    --Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
    removeInstruction :: [Instruction] -> ([Instruction], BoolNum)
    removeInstruction lst = (setNull lst menor, getDirtyBit menor)
        where   menor = foldl1 (\men elem -> if elem < men then elem else men) lst
                setNull (x:xs) ins
                    | x == ins = Null:xs
                    | otherwise = x : setNull xs ins
                                  
    replaceInstrucion :: Instruction -> RAM ->  RAM
    replaceInstrucion instr ram
            | bool == One = (incWriteNum . (addInstruction instr)) ram {getInstructions = nInstrs}
            | otherwise = (addInstruction instr) ram {getInstructions = nInstrs}
        where 
        (nInstrs, bool) = removeInstruction $ getInstructions ram
    
        
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
    
    
    loadInstruction :: RAM -> RawInstruction -> RAM
    loadInstruction ram rinstr = putInstructionInRAM (putInstructionInRAM ram (ft)) (sd)
            where   ft = fromRawInstruction First rinstr 
                    sd = fromRawInstruction Second rinstr
    
    loadInstructions :: RAM -> [RawInstruction] -> RAM
    loadInstructions ram [] = ram
    loadInstructions ram (x:xs) = loadInstructions (loadInstruction ram x) xs 
                