{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies, ScopedTypeVariables #-} 
-- TODO: Clean up above, not all of those are needed anymore

module Obsidian.GCDObsidian.CodeGen.CUDA (genKernel) where 

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut 


import qualified Obsidian.GCDObsidian.Tuple as Tuple 
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil) ) 
import Obsidian.GCDObsidian.Elem


----------------------------------------------------------------------------
-- -- TODO MAke this work with PUSHY arrays
----------------------------------------------------------------------------

gc = genConfig "" ""



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
    typeList ((a,t):xs)      = (genType gc t ++ a) : typeList xs
  
  
------------------------------------------------------------------------------    
-- CUDA Code from PKernel
    
genKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel name kernel a = cuda 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,_),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem  (Map.empty)
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res nosync) (0,[])

    c' = sc `mappend` (outCode `Seq` Skip) 
    sc = syncPoints c 
    
    cuda = getCUDA (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs)


getCUDA :: Config -> Code a -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getCUDA conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         tidLine >> newline >>
         bidLine >> newline >>
         sBase >> newline >> 
         genCUDABody conf c >>
         end ) 0 


genCUDABody :: Config -> Code a -> PP () 
genCUDABody _ Skip  = return () 
genCUDABody conf (su `Seq` code) = 
  do 
    genPSyncUnit conf su
    genCUDABody conf code  

genPSyncUnit conf (SyncUnit nt progs e) = 
  do 
    case compare nt blockSize of 
      LT -> do
            cond gc mm (tid <* (fromIntegral nt))
            begin
            mapM_ (genProg mm nt) progs
            end
      EQ -> mapM_ (genProg mm nt) progs
      GT -> error "genStore: CUDA code generation is broken somewhere" 

    where 
      mm = configMM conf
      blockSize = configThreads conf


genProg :: MemMap -> Word32 ->  Program -> PP () 
genProg mm nt (Assign name ix a) = 
  case Map.lookup name mm of 
    Just (addr,t) -> 
      do
        line$  sbaseStr addr t ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";" 
        newline
    Nothing ->  --- A result array
      do
        line$  name ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";"
        newline
genProg mm nt (ForAll f n) = genProg mm nt (f (variable "tid"))
genProg mm nt (Allocate name size t prg) = genProg mm nt prg
genProg mm nt (ProgramSeq p1 p2) = 
  do 
    genProg mm nt p1
    genProg mm nt p2
    
sbaseStr addr t = genCast gc t ++ "(sbase + " ++ show addr ++ ")" 