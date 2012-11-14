
module Obsidian.GCDObsidian.CodeGen.CUDA 
       (genKernel
       ,genKernel_ ) where  
     --  ,genKernelGlob
     --  ,genKernelGlob_ ) where 

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Exp 

import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
--import Obsidian.GCDObsidian.Program

import Obsidian.GCDObsidian.CodeGen.PP
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut 
import Obsidian.GCDObsidian.CodeGen.SyncAnalysis
import Obsidian.GCDObsidian.CodeGen.Memory
import Obsidian.GCDObsidian.CodeGen.Liveness

-- New imports
import Obsidian.GCDObsidian.CodeGen.Program 
import qualified Obsidian.GCDObsidian.Program as P 

import Obsidian.GCDObsidian.CodeGen.SPMDC

{- 
   TODO: 
    + phase out the old string based codegen 
    + Ideally there should be a Program -> SPMDC
      and SPMDC -> CUDA
          SPMDC -> OpenCL
          SPMDC -> X
      functions. 
-} 

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
  
---------------------------------------------------------------------------
-- genKernel 
---------------------------------------------------------------------------
genKernel :: ToProgram a b => String -> (a -> b) -> Ips a b -> String 
genKernel name kernel a = proto ++ cuda 
  where
    (ins,prg) = toProgram 0 kernel a

    -- collect outputs and extract the "actual" kernel
    outs = collectOutputs prg
    kern = extract prg 
    
    lc  = liveness kern -- tmpc
    
    -- Creates (name -> memory address) map      
    (m,mm) = mapMemory lc sharedMem Map.empty

    threadBudget =
      case prg of
        Skip -> error "empty programs not yet supported" -- gcdThreads res
        a    -> threadsPerBlock prg 

    proto = getProto name ins outs 
    cuda = getCUDA (config threadBudget mm (size m))
                   kern
                   name
                   ins outs

genKernel_ :: ToProgram a b => String -> (a -> b) -> Ips a b -> String 
genKernel_ name kernel a = {-proto ++ -} cuda
  where
    (ins,prg) = toProgram 0 kernel a
    -- collect outputs and extract the "actual" kernel
    outs = collectOutputs prg
    kern = extract prg 
    lc  = liveness kern 
    (m,mm) = mapMemory lc sharedMem Map.empty
    threadBudget =
      case prg of
        Skip -> error "empty programs not yet supported" -- gcdThreads res
        a    -> threadsPerBlock prg 

    spmd = performCSE2  (progToSPMDC threadBudget kern)
    body = shared : mmSPMDC mm spmd


    swap (x,y) = (y,x)
    inputs = map ((\(t,n) -> (typeToCType t,n)) . swap) ins
    outputs = map ((\(t,n) -> (typeToCType t,n)) . swap) outs 
    
    ckernel = CKernel CQualifyerKernel CVoid name (inputs++outputs) body
    shared = CDecl (CQualified CQualifyerExtern (CQualified CQualifyerShared ((CQualified (CQualifyerAttrib (CAttribAligned 16)) (CArray []  (CWord8)))))) "sbase"
    
    cuda = printCKernel (PPConfig "__global__" "" "" "__syncthreads()") ckernel 

---------------------------------------------------------------------------
-- genKernel_ (go via SPMDC and CSE) 
--------------------------------------------------------------------------- 
    {- 
genKernel_ :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel_ name kernel a = cuda 
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

    c' = c  *>* outCode 
    
    swap (x,y) = (y,x)
    inputs = map ((\(t,n) -> (typeToCType t,n)) . swap . fst2) ins
    outputs = map ((\(t,n) -> (typeToCType t,n)) . swap . fst2) outs 
    
   
    spmd = performCSE2  (progToSPMDC threadBudget c')
    body = shared : mmSPMDC mm spmd
    ckernel = CKernel CQualifyerKernel CVoid name (inputs++outputs) body
    shared = CDecl (CQualified CQualifyerExtern (CQualified CQualifyerShared ((CQualified (CQualifyerAttrib (CAttribAligned 16)) (CArray []  (CWord8)))))) "sbase"
    
    cuda = printCKernel (PPConfig "__global__" "" "" "__syncthreads()") ckernel 
  
    
    -- cuda = getCUDA (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs) 


---------------------------------------------------------------------------
-- Global array kernels
---------------------------------------------------------------------------
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
    --  + Maybe not an error. This kind of globalKernel 
    --    must push at some point since only push arrays are 
    --    acceptable global outputs
    threadBudget = threadsNeeded c 
    
    lc = liveness c_old 
    
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outcode,outs) = 
      runInOut_ (writeGlobalOutput threadBudget res) 
      
    c = c_old *>* outcode
     
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
    --       (This kind of kernel always will have) 
    threadBudget = threadsNeeded c 
    
    lc = liveness c_old 
    
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outcode,outs) = 
      runInOut_ (writeGlobalOutput threadBudget res) 
      
    c = c_old *>* outcode
    

    swap (x,y) = (y,x)
    inputs = map ((\(t,n) -> (typeToCType t,n)) . swap . fst2) ins
    outputs = map ((\(t,n) -> (typeToCType t,n)) . swap . fst2) outs 
    
    spmd = performCSE2 (progToSPMDC threadBudget c)
    body =  mmSPMDC mm spmd
    ckernel = CKernel CQualifyerKernel CVoid name (inputs++outputs) body
    cuda = printCKernel (PPConfig "__global__" "" "" "__syncthreads()") ckernel 
  
-} 
---------------------------------------------------------------------------
-- put together all the parts that make a CUDA kernel.
---------------------------------------------------------------------------
getCUDA :: Config 
          -- -> Code Syncthreads 
           -> Program a 
           -> Name 
           -> [(String,Type)] 
           -> [(String,Type)] 
           -> String 
           
getCUDA conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >> newline >> 
         -- tidLine >> newline >>
         -- bidLine >> newline >>
         sBase (configLocalMem conf) >> newline >> 
         genCUDABody conf c >>
         end ) 0
  
getProto :: Name -> [(String,Type)] -> [(String,Type)] -> String
getProto name ins outs =
  runPP (
    do 
      line "extern \"C\" "
      kernelHead name ins outs
      line ";"
      newline) 0 

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

---------------------------------------------------------------------------
-- pretty print a "Program", CUDA STYLE!
---------------------------------------------------------------------------
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
-- TODO: Add Similar AtomicOp case
--       Also see where else it needs to go!
--        # CSE
--        # Liveness
--
        
        
genProg mm nt (ForAll n f) = potentialCond gc mm n nt $ 
                               genProg mm nt (f (ThreadIdx X) {- (variable "tid") -} )
-- TODO: The following line is a HACK to make code generation 
---      for the histo function in Counting sort "work". 
--       More thought needed here. 
--genProg mm nt (ForAllGlobal f n) = genProg mm nt (f ((BlockIdx X * BlockDim X) + ThreadIdx X))
-- error "hello world"                                
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
    
ctid = cVar "tid" CWord32
  
progToSPMDC :: Word32 -> Program a -> [SPMDC] 
progToSPMDC nt (Assign name ix a) = 
  [cAssign (cVar name CWord8)[expToCExp ix] (expToCExp a)] 
progToSPMDC nt (ForAll n f) =         
  if (n < nt) 
  then 
    [cIf (cBinOp CLt ctid (cLiteral (Word32Val n) CWord32) CInt)
        code []]
  else 
    code 
  where 
    code = progToSPMDC nt (f (ThreadIdx X) {- (variable "tid") -} )
    
progToSPMDC nt (Allocate name size t _) = []
progToSPMDC nt (Synchronize True) = [CSync] 
progToSPMDC nt (Synchronize False) = [] 
progToSPMDC nt Skip = []
progToSPMDC nt (ProgramSeq p1 p2) = progToSPMDC nt p1 ++ progToSPMDC nt p2

----------------------------------------------------------------------------
-- generate a sbase CExpr

sbaseCExpr 0    = cVar "sbase" (CPointer CWord8) 
sbaseCExpr addr = cBinOp CAdd (cVar "sbase" (CPointer CWord8)) 
                              (cLiteral (Word32Val addr) CWord32) 
                              (CPointer CWord8) 
----------------------------------------------------------------------------
-- Memory map the arrays in an SPMDC
mmSPMDC :: MemMap -> [SPMDC] -> [SPMDC] 
mmSPMDC mm [] = [] 
mmSPMDC mm (x:xs) = mmSPMDC' mm x : mmSPMDC mm xs

mmSPMDC' :: MemMap -> SPMDC -> SPMDC
mmSPMDC' mm (CAssign e1 es e2) = 
  cAssign (mmCExpr mm e1) 
          (map (mmCExpr mm) es)    
          (mmCExpr mm e2)
mmSPMDC' mm (CFunc name es) = cFunc name (map (mmCExpr mm) es) 
mmSPMDC' mm CSync           = CSync
mmSPMDC' mm (CIf   e s1 s2) = cIf (mmCExpr mm e) (mmSPMDC mm s1) (mmSPMDC mm s2)
mmSPMDC' mm (CDeclAssign t nom e) = cDeclAssign t nom (mmCExpr mm e)
----------------------------------------------------------------------------
-- Memory map the arrays in an CExpr
mmCExpr mm (CExpr (CVar nom t)) =  
  case Map.lookup nom mm of 
    Just (addr,t) -> 
      let core = sbaseCExpr addr 
          cast c = cCast  c (typeToCType t)
      in cast core
    
    Nothing -> cVar nom t
mmCExpr mm (CExpr (CIndex (e1,es) t)) = cIndex (mmCExpr mm e1, map (mmCExpr mm) es) t
mmCExpr mm (CExpr (CBinOp op e1 e2 t)) = cBinOp op (mmCExpr mm e1) (mmCExpr mm e2) t
mmCExpr mm (CExpr (CUnOp op e t)) = cUnOp op (mmCExpr mm e) t 
mmCExpr mm (CExpr (CFuncExpr nom exprs t)) = cFuncExpr nom (map (mmCExpr mm) exprs) t
mmCExpr mm (CExpr (CCast e t)) = cCast (mmCExpr mm e) t 
mmCExpr mm a = a 
          
  
----------------------------------------------------------------------------
-- 
