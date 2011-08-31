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
-- Configuration that guides code generation 
data Config = Config {configThreads :: NumThreads, 
                      configMM      :: MemMap} 
config = Config


------------------------------------------------------------------------------
-- Generate CUDA code to a String 

getCUDA :: Config -> Code Syncthreads -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getCUDA conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         cudaBegin >>
         cudaTid >> newline >>
         cudaBid >> newline >>
         cudaSBase >> newline >> 
         genCUDABody conf c >>
         cudaEnd ) 0 


genCUDABody :: Config -> Code Syncthreads -> PP () 
genCUDABody _ Skip  = return () 
genCUDABody conf (Seq store code) = 
  do 
    genStoreConfig conf store 
    if storeNeedsSync store 
      then line "__syncthreads();" >> newline
      else return () 
    genCUDABody conf code  

------------------------------------------------------------------------------
-- Old 
genStore :: MemMap -> Store a -> PP () 
genStore mm (Store nt ws) = 
  do 
    cudaCond mm (tid <* (fromIntegral nt)) 
    cudaBegin
    mapM_ (genWrite mm nt) ws
    cudaEnd
                              
------------------------------------------------------------------------------
-- New
genStoreConfig :: Config -> Store a -> PP () 
genStoreConfig conf (Store nt ws) = 
  do 
    case compare nt blockSize of 
      LT -> do
            cudaCond mm (tid <* (fromIntegral nt))
            cudaBegin
            mapM_ (genWrite mm nt) ws
            cudaEnd
      EQ -> mapM_ (genWrite mm nt) ws
      GT -> error "genStore: CUDA code generation is broken somewhere" 

   

    
    where 
      mm = configMM conf
      blockSize = configThreads conf


genWrite :: MemMap -> Word32 -> Write a -> PP () 
genWrite mm nt (Write name ll _) = 
  sequence_  [let n  = fromIntegral nAssigns
                  ix = fromIntegral i 
              in cudaAssign mm (name (tid * n + ix))
                 (ll ! (tid * n + ix)) >> 
                 newline 
             | i <- [0..nAssigns-1]]
 
  where 
    nAssigns     = (staticLength ll) `div` nt 

  
cudaAssign :: Elem a => MemMap -> Exp a -> Exp a -> PP () 
cudaAssign mm name val = line ((concat (genExp mm name)) ++ 
                         " = " ++  concat (genExp mm val) ++ 
                         ";") 
                                                    
cudaCond :: MemMap -> Exp Bool -> PP ()  
cudaCond mm e = line ("if " ++ concat (genExp mm e))  

cudaBegin :: PP () 
cudaBegin = line "{" >> indent >> newline

cudaEnd :: PP () 
cudaEnd =  unindent >> newline >> line "}" >> newline


cudaTid = line "unsigned int tid = threadIdx.x;"
cudaBid = line "unsigned int bid = blockIdx.x;" 

cudaSBase = line "extern __shared__ unsigned char sbase[];" 



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
  
    ((res,s),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem (Map.empty)
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res nosync) (0,[])
      
    c' = sc +++ outCode
    sc = syncPoints c 
    
    cuda = getCUDA (config threadBudget mm) c' name ins outs
    
