module Version1 where

import Instruction
import RAM

addInstruction :: Instruction -> RAM -> RAM
addInstruction instr (RAM instrs faults refs counter) = RAM (instr:instrs) faults refs counter

--Devuelve la lista de instrucciones y ademas el dirty bit de la instrucción que se eliminó
removeInstruction :: [Instruction] -> ([Instruction], BoolNum)
removeInstruction lst
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

replaceInstrucion :: Instruction -> RAM ->  RAM
replaceInstrucion instr ram@(RAM instrs faults refs counter)
        | bool == One = (incDisksRefs . (addInstruction instr)) (RAM nInstrs faults refs counter)
        | otherwise = (addInstruction instr) (RAM nInstrs faults refs counter)
    where 
    (nInstrs, bool) = removeInstruction instrs

--Devuelve la instruccion que se elimino, ademas su dirty bit
getMinimalInstr :: [Instruction] -> (Instruction, BoolNum)
getMinimalInstr lst@(x:xs) =  (eliminated , getDirtyBit eliminated)
        where 
            eliminated = (foldr1 (\x acc -> if x < acc then x else acc)) lst

loadInstruction :: RAM -> Instruction -> RAM
loadInstruction ram instr
    --Si la instruccion esta en la RAM
    | isInstructionInRam nRam instr = nRam
        
    --Si la instruccion no esta en la RAM
    | otherwise = let nnRam = (incDisksRefs . incPageFaults) nRam --Incrementamos el numero de fallos de pagina y el numero de refrencias a disco, falta analizar otra posible referencia a disco en replaceInstructionV1
            in
                --Analizar si hay espacio en la RAM
                case (length (getInstructions nnRam)) of    32 -> replaceInstrucion instr nnRam
                                                            _  -> addInstruction instr nnRam

    where
    nRam = updateInstrCounter ram
            
loadInstructions :: RAM -> [Instruction] -> RAM
loadInstructions ram [] = ram
loadInstructions ram (x:xs) = loadInstructions (loadInstruction ram x) xs 
            