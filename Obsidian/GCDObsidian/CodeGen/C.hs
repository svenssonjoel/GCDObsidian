{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies #-} 

module Obsidian.GCDObsidian.CodeGen.C where 

import Data.List
import Data.Word 
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp  
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut

import qualified Obsidian.GCDObsidian.Tuple as Tuples
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil))
import Obsidian.GCDObsidian.Elem



------------------------------------------------------------------------------
-- sequential C code generation

getC :: Config -> Code a -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getC conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         sBase (configLocalMem conf) >> newline >> 
         genCBody conf c >>
         free_sBase >>
         end ) 0 

------------------------------------------------------------------------------

genCBody :: Config -> Code a -> PP () 
genCBody _ Skip  = return () 
genCBody conf (Seq store code) = 
  do 
    genStore conf store 
    genCBody conf code  

------------------------------------------------------------------------------
-- New
genStore :: Config -> Store a -> PP () 
genStore conf (Store nt ws) = 
  do 
    forEach mm (fromIntegral nt) 
    begin
    mapM_ (genWrite mm nt) ws
    end

    where 
      mm = configMM conf
      blockSize = configThreads conf

forEach :: MemMap -> Exp Word32 -> PP ()   
forEach mm e = line ("for (uint32_t tid = 0; tid < " ++ concat (genExp mm e) ++"; ++tid)")

------------------------------------------------------------------------------

genWrite :: MemMap -> Word32 -> Write a -> PP () 
genWrite mm nt (Write name ll _) = 
  sequence_  [let n  = fromIntegral nAssigns
                  ix = fromIntegral i 
              in assign mm (name (tid * n + ix))
                 (ll ! (tid * n + ix)) >> 
                 newline 
             | i <- [0..nAssigns-1]]
 
  where 
    nAssigns     = (staticLength ll) `div` nt 

------------------------------------------------------------------------------
    
sBase size = 
  do 
    line "unsigned char *sbase;" 
    newline  
    line ("sbase = (unsigned char*) malloc(" ++ show size ++ ");")
             

free_sBase = line "free(sbase);" 


------------------------------------------------------------------------------
-- C style function "header"
kernelHead :: Name -> 
              [(String,Type)] -> 
              [(String,Type)] -> 
              PP () 
kernelHead name ins outs = 
  do 
    line ("void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType t ++ a) : typeList xs
  
  
------------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable C kernel

genCKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genCKernel name kernel a = seqc 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,(_,mapArraySize)),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem mapArraySize(Map.empty)
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res ()) (0,[])
      
    c' = c +++ outCode
    -- sc = syncPoints c 
    
    seqc = getC (config threadBudget mm (size m)) c' name (("bid",Word32):(map fst2 ins)) (map fst2 outs)
    
