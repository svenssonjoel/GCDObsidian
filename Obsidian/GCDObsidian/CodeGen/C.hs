{-# LANGUAGE GADTs, RankNTypes, TypeOperators, TypeFamilies #-} 

module Obsidian.GCDObsidian.CodeGen.C where 

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

gc = genConfig "" ""

------------------------------------------------------------------------------
-- sequential C code generation

getC :: Config -> Code a -> Name -> [(String,Type)] -> [(String,Type)] -> String 
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
    
genSyncUnit :: Config -> SyncUnit a -> PP ()     
genSyncUnit conf (SyncUnit nt stores) = 
  do 
    genStoreList conf nt stores
  
genStoreList conf nt StoreListNil = return ()
genStoreList conf nt (StoreListCons s rest) = 
  do 
    genStore conf nt s 
    genStoreList conf nt rest
    
genStore :: Config -> Word32 -> Store a extra -> PP () 
genStore conf nt (Store name size ws) = 
  do 
    forEach gc mm (fromIntegral nt) 
    begin
    mapM_ (genWrite mm nt name) ws
    end

    where 
      mm = configMM conf
      blockSize = configThreads conf

forEach :: GenConfig -> MemMap -> Exp Word32 -> PP ()   
forEach gc mm e = line ("for (uint32_t tid = 0; tid < " ++ concat (genExp gc mm e) ++"; ++tid)")

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------
    
sBase size = 
  do 
    line "unsigned char *sbase;" 
    newline  
    line ("sbase = (unsigned char*) malloc(" ++ show size ++ ");")
             

free_sBase = line "free(sbase);" 


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
      
    c' = c +++ (code$ outCode)
    -- sc = syncPoints c 
    
    seqc = getC (config threadBudget mm (size m)) c' name (("bid",Word32):(map fst2 ins)) (map fst2 outs)
    
