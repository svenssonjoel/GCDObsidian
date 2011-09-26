{- 
   CodeGen.C 
   
   Generate C99 mostly for testing purposes and fun. 
-} 


module Obsidian.GCDObsidian.CodeGen.C where 

import Data.List
import Data.Word 
import Data.Monoid
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp  
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut

----------------------------------------------------------------------------
-- 

gc = genConfig "" ""

    
sBase size = 
  do 
    line "unsigned char *sbase;" 
    newline  
    line ("sbase = (unsigned char*) malloc(" ++ show size ++ ");")
             
free_sBase = line "free(sbase);" 

-- TODO: make more general and move to Common    
forEach :: GenConfig -> MemMap -> Exp Word32 -> PP ()   
forEach gc mm e = line ("for (uint32_t tid = 0; tid < " ++ concat (genExp gc mm e) ++"; ++tid)")

-- TODO: DUPLICATED CODE 
sbaseStr 0 t    = parens$ genCast gc t ++ "sbase" 
sbaseStr addr t = parens$ genCast gc t ++ "(sbase + " ++ show addr ++ ")" 
------------------------------------------------------------------------------
-- sequential C code generation

getC :: Config 
        -> Code a 
        -> Name 
        -> [(String,Type)] 
        -> [(String,Type)] 
        -> String 
        
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
genCBody conf (su `Seq` code) = 
  do 
    genSyncUnit conf su
    genCBody conf code  

------------------------------------------------------------------------------
-- New
    
genSyncUnit conf (SyncUnit nt progs e) = 
  do 
   forEach gc mm (fromIntegral nt) 
   begin
   mapM_ (genProg mm nt) progs
   end
  where 
    mm = configMM conf


----------------------------------------------------------------------------
-- pretty print a "Program", now C STYLE! 
-- But it is the same ??? 
-- TODO: DUPLICATED CODE 
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
    line ("void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType gc t ++ a) : typeList xs
  
  
------------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable C kernel

genKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel name kernel a = seqc 
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
      runInOut (writeOutputs threadBudget res ()) (0,[])
      
    c' = c +++ (code outCode)
    
    seqc = getC (config threadBudget mm (size m)) 
                c' 
                name 
                (("bid",Word32):(map fst2 ins)) 
                (map fst2 outs)
    
