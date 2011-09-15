{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies #-} 

module Obsidian.GCDObsidian.CodeGen.OpenCL where 

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
    genOpenCLBody conf code  
  
------------------------------------------------------------------------------
-- New
genSyncUnit :: Config -> SyncUnit Syncthreads -> PP ()
genSyncUnit conf (SyncUnit nt stores) =     
  do
    genStoreList conf nt stores 
    if storeListNeedsSync stores 
      then line "barrier(CLK_LOCAL_MEM_FENCE);" >> newline
      else return () 

genStoreList conf nt StoreListNil = return () 
genStoreList conf nt (StoreListCons s rest) = 
  do 
    genStore conf nt s 
    genStoreList conf nt rest


genStore :: Config -> Word32 -> Store a extra -> PP () 
genStore conf nt (Store name size ws) = 
  do 
    case compare nt blockSize of 
      LT -> do
            cond mm (tid <* (fromIntegral nt))
            begin
            mapM_ (genWrite mm nt name) ws
            end
      EQ -> mapM_ (genWrite mm nt name) ws
      GT -> error "genStore: OpenCL code generation is broken somewhere" 
    
    where 
      mm = configMM conf
      blockSize = configThreads conf


genWrite :: MemMap -> Word32 -> Name -> Write a extra -> PP () 
genWrite mm nt name (Write targf ll _) = 
  sequence_  [let n  = fromIntegral nAssigns
                  ix = fromIntegral i 
              in assign mm (index name (targf (tid * n + ix)))
                 (ll `llIndex` (tid * n + ix)) >> 
                 newline 
             | i <- [0..nAssigns-1]]
 
  where 
    nAssigns     = (staticLength ll) `div` nt 

  

tidLine = line "unsigned int tid = get_local_id(0);"

-- To get something that corresponds to bid in OpenCL 
-- you need the "blocksize" 
bidLine = line "unsigned int bid = (get_global_id(0)-tid) / get_local_size(0)" 

--here the shared memory size is needed (I think) 
sBase size = line$ "__local unsigned char sbase[" ++ show size ++ "];" 



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
    typeList ((a,t):xs)      = (genType (Global t) ++ a) : typeList xs
  
  
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
    
