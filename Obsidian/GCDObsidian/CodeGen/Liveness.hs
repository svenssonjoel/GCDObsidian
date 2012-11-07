
module Obsidian.GCDObsidian.CodeGen.Liveness where

import qualified Data.Set as Set

import Obsidian.GCDObsidian.Exp
-- import Obsidian.GCDObsidian.Array
--import Obsidian.GCDObsidian.Memory
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.CodeGen.Program

------------------------------------------------------------------------------
-- LIVENESS on Programs

type Liveness = Set.Set Name

liveness :: Program e -> Program Liveness
liveness p = fst$ liveness' p Set.empty 

liveness' :: Program e -> Liveness -> (Program Liveness,Liveness) 
liveness' (Assign name ix exp ) s = (Assign name ix exp, living)
  where 
    arrays = collectArrays exp 
    living    = Set.fromList arrays `Set.union` s
   
liveness' (Allocate name size t _) s = 
  (Allocate name size t alive,alive)
  where 
    alive = name `Set.delete` s
  
liveness' (ForAll n ixfToPrg) s = (ForAll n (fst . ixf'),living)    
  where 
    ixf' = ((flip liveness') Set.empty) . ixfToPrg
    aliveInside = snd$ ixf' (variable "X") 
    living = s `Set.union` aliveInside
    -- NOTE: Performs local liveness check (to what use ?) 
--liveness' e@(Cond b p) s = (Cond b p',s') 
--  where 
--    (p',s') = liveness' p s 
  -- TODO: Change this if the conditions depend on 
  -- previously computed arrays 
  -- NOTE: Need to traverse p here just to shift its type to Program Liveness

liveness' (Synchronize b) s = (Synchronize b,s) 
liveness' Skip s = (Skip, s)

liveness' (p1 `ProgramSeq` p2) s = 
  (p1' `ProgramSeq` p2',l1) 
  where 
    (p2',l2) = liveness' p2 s
    (p1',l1) = liveness' p1 l2


 

{- 
liveness :: Code a -> Code Liveness 
liveness (s `Seq` c) = lives `Seq` livec 
    where 
      lives = livenessSyncUnit aliveNext s  
      livec = liveness c
      aliveNext = whatsAliveNext livec

liveness Skip = Skip 
-} 
{- 
livenessSyncUnit aliveNext (SyncUnit nt prg _) = 
  SyncUnit nt prg alive
  where alive = livenessProgram aliveNext prg


livenessProgram aliveNext (Assign name ix e)  = livingArrs
  where 
    arrays = collectArrays e
    tmp    = Set.fromList arrays `Set.union` aliveNext
    livingArrs = name `Set.delete` tmp
livenessProgram aliveNext (ForAll f n) = livenessProgram aliveNext (f (variable "X"))    
livenessProgram aliveNext (Allocate name size t prg) = livenessProgram aliveNext prg
livenessProgram aliveNext (Cond c p) = livenessProgram aliveNext p
livenessProgram aliveNext (prg1 `ProgramSeq` prg2) = 
  livenessProgram aliveNext prg1 `Set.union` livenessProgram aliveNext prg2
-}


-- TODO: Think about what kind of programs there 
--       will be. Is the below correct ? 
whatsAliveNext :: Program Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (Synchronize _) = Set.empty
whatsAliveNext (Allocate _ _ _ l) = l 
whatsAliveNext (Assign _ _ _) = Set.empty
whatsAliveNext (ForAll _{-p-} _) = Set.empty 
-- I dont think any programs we generate will 
-- allocate things within a forAll! 
-- (Make this more clear in the type ?)
-- whatsAliveNext (Cond _ _ _ p) = whatsAliveNext p
-- Most likely Cond will not contain Alloc nodes
-- beneath it either. 
whatsAliveNext (p1 `ProgramSeq` p2) = 
  let s1 = whatsAliveNext p1  
      s2 = whatsAliveNext p2 
  in s1 `Set.union` s2

{-
whatsAliveNext :: Code Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (s `Seq` _) = syncExtra s
-}
------------------------------------------------------------------------------ 




------------------------------------------------------------------------------
-- Create a memory map on CODE 
    {- 
mapMemory :: Code Liveness -> Memory -> MemMap -> (Memory,MemMap) 
mapMemory Skip m mm = (m,mm) 
mapMemory (su `Seq` code) m mm = mapMemory code m' mm' 
  where 
    (m'',mm') = mapMemorySyncUnit su m mm 
    aliveNext = whatsAliveNext code
    aliveNow  = syncExtra su 
    diff      = aliveNow Set.\\ aliveNext
    diffAddr  = mapM (\x -> Map.lookup x mm') (filter (not . (isPrefixOf "input")) (Set.toList diff))
    m'        = 
      case diffAddr of 
        (Just addys) -> freeAll m'' (map fst addys)
        Nothing      -> error "atleast one array does not exist in memorymap" 
     
    
    
mapMemorySyncUnit (SyncUnit nt p e) m mm  = mapMemoryProgram p m mm 

-} 
 