{- 
   CodeGen.OpenCL

   OpenCL code generation.
-} 

module Obsidian.GCDObsidian.CodeGen.OpenCL (genKernel) where 

import Data.List
import Data.Word 
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp  
import Obsidian.GCDObsidian.Memory
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut



----------------------------------------------------------------------------
-- When it comes to OpenCL the gc is slightly different ;) 
gc = genConfig "__global" "__local"

syncLine = line "barrier(CLK_LOCAL_MEM_FENCE);"

tidLine = line "unsigned int tid = get_local_id(0);"

-- To get something that corresponds to bid in OpenCL 
-- you need the "blocksize" 
bidLine = line "unsigned int bid = (get_global_id(0)-tid) / get_local_size(0);" 

-- Here the shared memory size is needed (I think) 
-- Note:  You can set the size here (in the kernel) or 
--        from the outside. Setting it from the "outside" requires an 
--        extra parameter passed to the kernel and is thus more cumbersome.
sBase size = line$ "__local unsigned char sbase[" ++ show size ++ "];" 

-- TODO: CODE DUPLICATION
sbaseStr 0 t    = parens$ genCast gc t ++ "sbase" 
sbaseStr addr t = parens$ genCast gc t ++ "(sbase + " ++ show addr ++ ")" 

------------------------------------------------------------------------------
-- Generate OpenCL code to a String 

getOpenCL :: Config -> Code Syncthreads -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getOpenCL conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         tidLine >> newline >>
         bidLine >> newline >>
         sBase (configLocalMem conf) >> newline >> 
         genOpenCLBody conf c >>
         end ) 0 


genOpenCLBody :: Config -> Code Syncthreads -> PP () 
genOpenCLBody _ Skip  = return () 
genOpenCLBody conf (su `Seq` code) = 
  do 
    genSyncUnit conf su
    if (syncUnitNeedsSync su) 
      then syncLine >> newline 
      else return () 
    genOpenCLBody conf code  
  
------------------------------------------------------------------------------
-- 
-- TODO: CODE DUPLICATION !!! 
genSyncUnit conf (SyncUnit nt prog e) = 
  do 
    case compare nt blockSize of 
      LT -> do
            cond gc mm (tid <* (fromIntegral nt))
            begin
            genProg mm nt prog
            end
      EQ -> genProg mm nt prog
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
genProg mm nt (Cond c p) = 
  line ("if" ++ concat (genExp gc mm c)) >> begin >>
  genProg mm nt p >>
  end 


------------------------------------------------------------------------------
-- C style function "header"
kernelHead :: Name -> 
              [(String,Type)] -> 
              [(String,Type)] -> 
              PP () 
kernelHead name ins outs = 
  do 
    line ("__kernel void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType gc (Global t) ++ a) : typeList xs
  
  
------------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable OpenCL kernel

genKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel name kernel a = opencl 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,_),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res nosync) (0,[])
      
    c' = sc +++ (code outCode)
    sc = syncPoints c 
    
    opencl = getOpenCL (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs)
    
