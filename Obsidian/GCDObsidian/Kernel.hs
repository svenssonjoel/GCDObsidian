
module Obsidian.GCDObsidian.Kernel where 

import Control.Monad  
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Data.Word
import Data.List 
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Map as Map
import Prelude hiding (elem)

import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Memory
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program

------------------------------------------------------------------------------  
-- threadId, here or elsewhere ?

tid :: Exp Word32
tid = variable "tid"
      
type NumThreads = Word32      
      

------------------------------------------------------------------------------
-- Kernels

--data SyncUnit extra = SyncUnit {syncThreads  :: Word32, 
--                                syncProgram  :: Program,
--                                syncExtra    :: extra }
--                 deriving Show
                          
--syncUnit :: Word32 -> Program -> SyncUnit ()                          
--syncUnit w p = SyncUnit w p ()


--data Code extra = Skip 
--                | (SyncUnit extra) `Seq` (Code extra) 
--                deriving Show                    
                   
                         
--code :: SyncUnit a -> Code a 
--code su = su `Seq` Skip                          

--(+++) :: Code a -> Code a -> Code a 
--(+++) = mappend
                          
--instance Monoid (Code extra) where                    
--  mempty = Skip
--  mappend Skip a = a 
--  mappend a Skip = a 
--  mappend (ps `Seq` c) c2 = ps `Seq` (mappend c c2)
                   
type Kernel a = StateT Integer (Writer (Program ())) a   

runKernel k = runWriter (runStateT k 0)

newArray :: Kernel Name 
newArray  = do
  i <- get
  let newName = "arr" ++ show i 
  put (i+1)
  return newName


(->-) :: (a -> Kernel b) -> (b -> Kernel c) -> (a -> Kernel c) 
(->-) = (>=>) 

pure f a = return (f a) 

----------------------------------------------------------------------------
-- Figure out how many threads a piece of Code requires
threadsNeeded :: Program e -> Word32
threadsNeeded = programThreads 

------------------------------------------------------------------------------
-- LIVENESS on CODE 

type Liveness = Set.Set Name

liveness :: Code a -> Code Liveness 
liveness (s `Seq` c) = lives `Seq` livec 
    where 
      lives = livenessSyncUnit aliveNext s  
      livec = liveness c
      aliveNext = whatsAliveNext livec

liveness Skip = Skip 

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



whatsAliveNext :: Code Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (s `Seq` _) = syncExtra s

------------------------------------------------------------------------------ 




------------------------------------------------------------------------------
-- Create a memory map on CODE 
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


mapMemoryProgram (Assign name i a) m mm = (m,mm) 
mapMemoryProgram (ForAll f n) m mm = mapMemoryProgram (f (variable "X")) m mm       
mapMemoryProgram (Cond c p) m mm = mapMemoryProgram p m mm 
mapMemoryProgram (Allocate name size t program) m mm = mapMemoryProgram program m' mm'
  where 
    (m'',addr) = allocate m size
    -- TODO: maybe create Global arrays if Local memory is full.
    -- t = Pointer$ Local$ typeOf$ getLLArray (head ws) `llIndex`  tid
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -
mapMemoryProgram (prg1 `ProgramSeq` prg2) m mm = mapMemoryProgram prg2 m' mm'
  where 
    (m',mm') = mapMemoryProgram prg1 m mm 

