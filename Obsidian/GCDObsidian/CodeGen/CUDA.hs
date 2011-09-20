{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies, ScopedTypeVariables #-} 

module Obsidian.GCDObsidian.CodeGen.CUDA (genPKernel, genKernel, getCUDA ) where 

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
-- TODO: Move the conditionals into genSyncUnit. That is a partial solution 
--       to the TODO item above as well

------------------------------------------------------------------------------
-- Generate CUDA code to a String 

gc = genConfig "" ""

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
genCUDABody conf (su `Seq` code) = 
  do 
    genSyncUnit conf su
    genCUDABody conf code  

        
------------------------------------------------------------------------------
-- New
    
genSyncUnit :: Config -> SyncUnit Syncthreads -> PP ()
genSyncUnit conf (SyncUnit nt stores) = 
  do 
    genStoreList conf nt stores
    if storeListNeedsSync stores 
      then line "__syncthreads();" >> newline
      else return () 
        
genStoreList conf nt StoreListNil = return ()
genStoreList conf nt (StoreListCons s rest) = 
  do 
    genStore conf nt s  
    genStoreList conf nt rest
    
--TODO: Maybe the conditional here can be moved out into genSyncUnit
genStore :: Config -> Word32 -> Store a extra -> PP () 
genStore conf nt (Store name size ws) = 
  do 
    case compare nt blockSize of 
      LT -> do
            cond gc mm (tid <* (fromIntegral nt))
            begin
            mapM_ (genWrite mm nt name) ws
            end
      EQ -> mapM_ (genWrite mm nt name) ws
      GT -> error "genStore: CUDA code generation is broken somewhere" 

    where 
      mm = configMM conf
      blockSize = configThreads conf


genWrite :: MemMap -> Word32 -> Name -> Write a extra -> PP () 
genWrite mm nt name (Write targf ll _) = 
  sequence_  [let n  = fromIntegral nAssigns
                  ix = fromIntegral i 
              in assign gc mm (index name (targf (tid * n + ix)))
                 (ll `llIndex` (tid * n + ix)) >> 
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
    typeList ((a,t):xs)      = (genType gc t ++ a) : typeList xs
  
  
------------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable CUDA kernel

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
      
    c' = sc +++ (code$ outCode)
    sc = syncPoints c 
    
    cuda = getCUDA (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs)
    
------------------------------------------------------------------------------    
-- CUDA Code from PKernel
    
    
genPKernel :: (InOut a, InOut b) => String -> (a -> PKernel b) -> a -> String 
genPKernel name kernel a = cuda 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,_),c)  = runPKernel (kernel input)
    lc = livenessP c
   
    threadBudget = 
      case c of 
        PSkip -> gcdThreads res
        a  -> threadsNeededP c 
        
    (m,mm) = mapMemoryP lc sharedMem  (Map.empty)
    (outCode,outs)   = 
      runInOut (writeOutputsP threadBudget res nosync) (0,[])

    c' = sc `mappend` (outCode `PSeq` PSkip) 
    sc = syncPointsP c 
    
    cuda = getCUDAP (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs)


getCUDAP :: Config -> PCode a -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getCUDAP conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         tidLine >> newline >>
         bidLine >> newline >>
         sBase >> newline >> 
         genCUDABodyP conf c >>
         end ) 0 


genCUDABodyP :: Config -> PCode a -> PP () 
genCUDABodyP _ PSkip  = return () 
genCUDABodyP conf (su `PSeq` code) = 
  do 
    genPSyncUnit conf su
    genCUDABodyP conf code  

genPSyncUnit conf (PSyncUnit nt progs e) = 
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
          concat (genExp gc mm a) ++ ";\n"
        newline
genProg mm nt (ForAll f n) = genProg mm nt (f (variable "tid"))
genProg mm nt (Allocate name size t prg) = genProg mm nt prg
genProg mm nt (ProgramSeq p1 p2) = 
  do 
    genProg mm nt p1
    genProg mm nt p2
    
sbaseStr addr t = genCast gc t ++ "(sbase + " ++ show addr ++ ")" 