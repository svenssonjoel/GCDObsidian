
module Obsidian.GCDObsidian.CodeGen.CUDA 
       (genKernel
       ,genKernel_
       ,genKernelGlob
       ,genKernelGlob_) where 

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map


import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp 

import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut 

import Obsidian.GCDObsidian.CodeGen.SyncAnalysis
import Obsidian.GCDObsidian.CodeGen.Memory
import Obsidian.GCDObsidian.CodeGen.Liveness

import Obsidian.GCDObsidian.CodeGen.SPMDC

----------------------------------------------------------------------------
-- 
gc = genConfig "" ""

syncLine = line "__syncthreads();"

tidLine = line "unsigned int tid = threadIdx.x;"
bidLine = line "unsigned int bid = blockIdx.x;" 


sBase size = if size > 0 
                then line "extern __shared__ __attribute__ ((aligned (16))) unsigned char sbase[];" 
                else return ()     
                     
sbaseStr 0 t    = parens$ genCast gc t ++ "sbase" 
sbaseStr addr t = parens$ genCast gc t ++ "(sbase + " ++ show addr ++ ")" 

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
-- CUDA Code from Kernel
  
genKernel_ :: (InOut b) 
              => String 
              -> Kernel b 
              -> [(String,Type)] -- Inputs
              -> [(String,Type)] -- Outputs
              -> (String,Word32,Word32)     
genKernel_ name kernel ins outs = (cuda,threads,size m) 
  where  
    ((res,_),prg) = runKernel kernel  -- generate the "Program"
    
    saPrg = syncAnalysis prg -- program that only syncs where needed
    
    lvPrg = liveness saPrg   -- program annotated with liveness information
    
    threads =  
      case saPrg of 
        Skip -> gcdThreads res
        a  -> threadsNeeded saPrg 
   
    -- Create a memory-map for the program 
    (m,mm) = mapMemory lvPrg sharedMem  (Map.empty)
    
    finalPrg = saPrg 
   
    cuda = getCUDA (config threads mm (size m)) 
                   finalPrg 
                   name 
                   ins
                   outs

        
      
    
genKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel name kernel a = cuda 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,_),c_old)  = runKernel (kernel input)
    
    c = syncAnalysis c_old  
    lc  = liveness c
    
    

   
    threadBudget =  
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem  (Map.empty)
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res {-nosync-}) (0,[])

    c' = sc {-*>* Synchronize True-} *>* outCode 
    sc = c -- remove
    
--    cuda = error$ printBody (mmSPMDC mm (progToSPMDC threadBudget c))
    cuda = getCUDA (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs)


genKernelGlob :: (GlobalInput a, GlobalOutput b)
                 => String 
                 -> (a -> Kernel b) 
                 -> a 
                 -> String
genKernelGlob name kernel a = cuda 
  where 
    (input,ins) = runInOut_ (createGlobalInput a)                             
    ((res,_),c_old) = runKernel (kernel input) 
    
    -- TODO: *ERROR* will only work if there 
    --       is atleast one sync in the kernel. 
    threadBudget = threadsNeeded c 
    
    lc = liveness c_old 
    
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outcode,outs) = 
      runInOut_ (writeGlobalOutput threadBudget res) 
      
    c = c_old *>* outcode
    
    -- Perform CSE here 
    -- Maybe output a new form of intermediate code 
    -- from this stage ?? 
    -- c' = performCSE c 
    
 --    cuda = error$ printBody (mmSPMDC mm (progToSPMDC threadBudget c) )
 
    cuda = getCUDA (config threadBudget mm (size m)) 
                   c  
                   name
                   (map fst2 ins) 
                   (map fst2 outs)
 


genKernelGlob_ :: (GlobalInput a, GlobalOutput b)
                 => String 
                 -> (a -> Kernel b) 
                 -> a 
                 -> String
genKernelGlob_ name kernel a = cuda 
  where 
    (input,ins) = runInOut_ (createGlobalInput a)                             
    ((res,_),c_old) = runKernel (kernel input) 
    
    -- TODO: *ERROR* will only work if there 
    --       is atleast one sync in the kernel. 
    threadBudget = threadsNeeded c 
    
    lc = liveness c_old 
    
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outcode,outs) = 
      runInOut_ (writeGlobalOutput threadBudget res) 
      
    c = c_old *>* outcode
    
    -- Perform CSE here 
    -- Maybe output a new form of intermediate code 
    -- from this stage ?? 
    -- c' = performCSE c 
    swap (x,y) = (y,x)
    inputs = map ((\(t,n) -> (typeToCType t,n)) . swap . fst2) ins
    outputs = map ((\(t,n) -> (typeToCType t,n)) . swap . fst2) outs 
    tidDecl = CDeclAssign CWord32 "tid" (CVar "threadIdx.x") 
    bidDecl = CDeclAssign CWord32 "bid" (CVar "blockIdx.x") 
    
    body = tidDecl:bidDecl:(mmSPMDC mm (progToSPMDC threadBudget c) )
    cuda = printCKernel$ CKernel CQualifyerKernel CVoid name (inputs++outputs) body
      {- 
    cuda = getCUDA (config threadBudget mm (size m)) 
                   c  
                   name
                   (map fst2 ins) 
                   (map fst2 outs)
     -} 



------------------------------------------------------------------------------
-- put together all the parts that make a CUDA kernel.     
getCUDA :: Config 
          -- -> Code Syncthreads 
           -> Program a 
           -> Name 
           -> [(String,Type)] 
           -> [(String,Type)] 
           -> String 
           
getCUDA conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         tidLine >> newline >>
         bidLine >> newline >>
         sBase (configLocalMem conf) >> newline >> 
         genCUDABody conf c >>
         end ) 0 


----------------------------------------------------------------------------
-- Code to a CUDA kernel Body
genCUDABody :: Config 
              -- -> Code Syncthreads 
               -> Program a 
               -> PP () 
-- genCUDABody _ Skip  = return () 
genCUDABody conf prg = genProg mm nt prg
   where 
      mm = configMM conf
      nt = configThreads conf

----------------------------------------------------------------------------
-- pretty print a "Program", CUDA STYLE!
genProg :: MemMap -> Word32 ->  Program a -> PP () 
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
        
        
genProg mm nt (ForAll f n) = potentialCond gc mm n nt $ 
                               genProg mm nt (f (variable "tid"))
-- Investigate why this line is not used. 
genProg mm nt (ForAllGlobal f n) = error "hello world"                                
--genProg mm nt (f (variable "gtid"))
  
  
  -- TODO: Many details missing here, think about nested ForAlls 
  -- TODO: Sync only if needed here                              
  --      ++ Might help to add information to Program type that a "sync is requested"                              
                             
genProg mm nt (Allocate name size t _) = return () -- genProg mm nt prg
genProg mm nt (Synchronize True) = syncLine >> newline 
genProg mm nt (Synchronize False) = return () -- line "\\\\__synchthreads();" >> newline 
genProg mm nt Skip = return ()
genProg mm nt (ProgramSeq p1 p2) = 
  do 
    genProg mm nt p1
    genProg mm nt p2



---------------------------------------------------------------------------- 
-- genProgSPMDC 
    

  
progToSPMDC :: Word32 -> Program a -> [SPMDC] 
progToSPMDC nt (Assign name ix a) = 
  [CAssign (CVar name) [expToCExp ix] (expToCExp a)] 
progToSPMDC nt (ForAll f n) =         
  if (n < nt) 
  then 
    [CIf (CBinOp CLt (CVar "tid") (CLiteral (Word32Val n)))
        code []]
  else 
    code 
  where 
    code = progToSPMDC nt (f (variable "tid"))
    
progToSPMDC nt (Allocate name size t _) = []
progToSPMDC nt (Synchronize True) = [CFunc "__synchthreads" []] -- syncLine >> newline 
progToSPMDC nt (Synchronize False) = [] -- return () -- line "\\\\__synchthreads();" >> newline 
progToSPMDC nt Skip = []
progToSPMDC nt (ProgramSeq p1 p2) = progToSPMDC nt p1 ++ progToSPMDC nt p2



mmSPMDC :: MemMap -> [SPMDC] -> [SPMDC] 
mmSPMDC mm [] = [] 
mmSPMDC mm (x:xs) = mmSPMDC' mm x : mmSPMDC mm xs

mmSPMDC' :: MemMap -> SPMDC -> SPMDC
mmSPMDC' mm (CAssign e1 es e2) = 
  CAssign (mmCExpr mm e1) 
          (map (mmCExpr mm) es)    
          (mmCExpr mm e2)
mmSPMDC' mm (CFunc name es) = CFunc name (map (mmCExpr mm) es) 
mmSPMDC' mm (CIf   e s1 s2) = CIf (mmCExpr mm e) (mmSPMDC mm s1) (mmSPMDC mm s2)

mmCExpr mm (CVar nom) =  
  case Map.lookup nom mm of 
    Just (addr,t) -> 
      let core = CBinOp CAdd (CVar "sbase") (CLiteral (Word32Val addr))
          cast c = CCast (typeToCType t) c
      in cast core
    
    Nothing -> CVar nom
mmCExpr mm (CIndex (e1,es)) = CIndex (mmCExpr mm e1, map (mmCExpr mm) es)
mmCExpr mm (CBinOp op e1 e2) = CBinOp op (mmCExpr mm e1) (mmCExpr mm e2)
mmCExpr mm (CUnOp op e) = CUnOp op (mmCExpr mm e) 
mmCExpr mm (CFuncExpr nom exprs) = CFuncExpr nom (map (mmCExpr mm) exprs)
mmCExpr mm (CCast t e) = CCast t (mmCExpr mm e) 
mmCExpr mm a = a 
          
  

typeToCType Int   = CInt
typeToCType Float = CFloat
typeToCType Double = CDouble
typeToCType Word8 = CWord8
typeToCType Word16 = CWord16
typeToCType Word32 = CWord32
typeToCType Word64 = CWord64
typeToCType (Pointer t) = CPointer (typeToCType t)
typeToCType (Global t)  = CQualified CQualifyerGlobal (typeToCType t) 
typeToCType (Local t)  = CQualified CQualifyerLocal (typeToCType t) 

