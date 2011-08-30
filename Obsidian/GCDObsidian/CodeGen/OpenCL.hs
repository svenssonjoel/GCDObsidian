{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies #-} 

module Obsidian.GCDObsidian.CodeGen.OpenCL where 

import Data.List
import Data.Word 
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp  
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.GodeGen.InOut

import qualified Obsidian.GCDObsidian.Tuple as Tuples
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil))
import Obsidian.GCDObsidian.Elem


------------------------------------------------------------------------------
-- 

data Config = Config {configThreads :: NumThreads, 
                      configMM      :: MemMap} 
config = Config



------------------------------------------------------------------------------
-- Generate OpenCL code to a String 

getOpenCL :: Config -> Code Syncthreads -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getOpenCL conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         cudaBegin >>
         cudaTid >> newline >>
         cudaBid >> newline >>
         cudaSBase >> newline >> 
         genOpenCLBody conf c >>
         cudaEnd ) 0 


genOpenCLBody :: Config -> Code Syncthreads -> PP () 
genOpenCLBody _ Skip  = return () 
genOpenCLBody conf (Seq store code) = 
  do 
    genStoreConfig conf store 
    if storeNeedsSync store 
      then line "__syncthreads();" >> newline
      else return () 
    genOpenCLBody conf code  

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
      GT -> error "genStore: OpenCL code generation is broken somewhere" 

   

    
    where 
      mm = configMM conf
      blockSize = configThreads conf


genWrite :: MemMap -> Int -> Write a -> PP () 
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

cudaSBase = line "extern __shared__ unsigned char sbase[]" 



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
-- Gives a string that should be a runnable OpenCL kernel

genOpenCLKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genOpenCLKernel name kernel a = cuda 
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
    
    cuda = getOpenCL (config threadBudget mm) c' name ins outs
    
