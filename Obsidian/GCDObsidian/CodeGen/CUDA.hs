{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies #-} 

module Obsidian.GCDObsidian.CodeGen.CUDA (genCUDAKernel, getCUDA ) where 

import Data.List
import Data.Word 
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut 


import qualified Obsidian.GCDObsidian.Tuple as Tuple 
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil) ) 
import Obsidian.GCDObsidian.Elem


------------------------------------------------------------------------------
-- TODO: Add Sync "analysis"
--         * DONE First: Add Sync everywhere ! 
--         * second: Only add when arrays are computed using more than one warp
--         * Third: Only sync when the communication pattern actually demands it         
--               (sort assignments based on dependency between eachother) 
--
-- DONE: Create proper kernel headers (kernelName (type1 a0, ... ) 
-- TODO: Remove unwanted conditionals 
--          * First by merging with previous if possible
--          * Can apply more effort if wanted and "sort" the assignments
--            In dependency order before doing merging. 


------------------------------------------------------------------------------
-- Generate CUDA code to a String 

getCUDA :: Config -> Code Syncthreads -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getCUDA conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         tidLine >> newline >>
         bidLine >> newline >>
         sBase >> newline >> 
         genCUDABody conf c >>
         end ) 0 


genCUDABody :: Config -> Code Syncthreads -> PP () 
genCUDABody _ Skip  = return () 
genCUDABody conf (Seq store code) = 
  do 
    genStore conf store 
    if storeNeedsSync store 
      then line "__syncthreads();" >> newline
      else return () 
    genCUDABody conf code  

        
------------------------------------------------------------------------------
-- New
genStore :: Config -> Store a -> PP () 
genStore conf (Store nt ws) = 
  do 
    case compare nt blockSize of 
      LT -> do
            cond mm (tid <* (fromIntegral nt))
            begin
            mapM_ (genWrite mm nt) ws
            end
      EQ -> mapM_ (genWrite mm nt) ws
      GT -> error "genStore: CUDA code generation is broken somewhere" 

    where 
      mm = configMM conf
      blockSize = configThreads conf


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


tidLine = line "unsigned int tid = threadIdx.x;"
bidLine = line "unsigned int bid = blockIdx.x;" 


sBase = line "extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];" 



------------------------------------------------------------------------------
-- C style function "header"
kernelHead :: Name -> 
              [(String,Type)] -> 
              [(String,Type)] -> 
              PP () 
kernelHead name ins outs = 
  do 
    line ("__global__ void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType t ++ a) : typeList xs
  
  
------------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable CUDA kernel

genCUDAKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genCUDAKernel name kernel a = cuda 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,(_,mapArraySize)),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem mapArraySize (Map.empty)
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res nosync) (0,[])
      
    c' = sc +++ outCode
    sc = syncPoints c 
    
    cuda = getCUDA (config threadBudget mm) c' name ins outs
    
