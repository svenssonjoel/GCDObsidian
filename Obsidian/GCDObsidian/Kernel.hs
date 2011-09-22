
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

import Obsidian.GCDObsidian.Elem
import qualified Obsidian.GCDObsidian.Tuple as Tuple
import Obsidian.GCDObsidian.Tuple (Tuple ((:.),Nil) ) 
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Array


------------------------------------------------------------------------------  
-- Tid. Probably not really needed here

tid :: Exp Word32
tid = variable "tid"
      
type NumThreads = Word32      
      

------------------------------------------------------------------------------
-- Pushy Kernels

data SyncUnit extra = SyncUnit {syncThreads  :: Word32, 
                                syncPrograms :: [Program],
                                syncExtra    :: extra }
                 deriving Show
                          
syncUnit :: Word32 -> [Program] -> SyncUnit ()                          
syncUnit w ps = SyncUnit w ps ()


data Code extra = Skip -- nothing to do 
                 | (SyncUnit extra) `Seq` (Code extra) 
                 deriving Show                    
                   
instance Monoid (Code extra) where                    
  mempty = Skip
  mappend Skip a = a 
  mappend a Skip = a 
  mappend (ps `Seq` c) c2 = ps `Seq` (mappend c c2)
                   
type Kernel a = StateT Integer (Writer (Code ())) a   

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
threadsNeeded :: Code e -> Word32
threadsNeeded Skip = 0
threadsNeeded (Seq (SyncUnit nt _ _) c) = 
  max nt (threadsNeeded c)

------------------------------------------------------------------------------
-- Memory layout

type MemMap = Map.Map Name (Word32,Type)

type Address = Word32
type Bytes   = Word32 

data Memory = Memory {freeList  :: [(Address,Bytes)] ,
                      allocated :: [(Address,Bytes)] , 
                      size      :: Bytes} -- how much used
              
              
-- 48 kilobytes of smem              
sharedMem = Memory [(0,49152)] [] 0

updateMax :: Memory -> Memory 
updateMax mem = let m = maximum [a+b|(a,b) <- allocated mem]
                    m' = max m (size mem)
                in mem {size = m'}

allocate :: Memory -> Bytes -> (Memory,Address)
allocate m b = 
  let adress = filter (\(x,y) -> y >= b) (freeList m) -- get a list of candidates
      getTop mem = let (a,b)  = case null (allocated m) of 
                         False -> maximum $ List.sort (allocated m) 
                         True  -> (0,0)
                   in a+b
  in case adress of 
    -- use the first candidate (try better approaches 
    --  such as searching for best match, so that not to waste memory)
    ((a,bytes):_)  -> let fl = filter (\(addr,_) -> a /= addr) (freeList m)
                          fl' = if b < bytes 
                                then (a+b,bytes-b):fl
                                else fl
                      in  (updateMax (m {freeList = fl', 
                                         allocated = (a,b):allocated m}) ,a)
                          
                          
                          
                          
free :: Memory -> Address -> Memory
free m a = mem 
    where 
      bytes = lookup a (allocated m)
      al    = filter (\(addr,_) -> a /= addr) (allocated m)
      
      merge [] = [] 
      merge [x] = [x]
      merge ((x,b):(y,b2):xs) = if (x+b == y) then merge ((x,b+b2):xs) 
                                              else (x,b):merge((y,b2):xs)
      mem   = case bytes of 
                Nothing -> error $ "error: Address " ++ show a ++ " not found in free list"
                Just b -> m {freeList = compress ((a,b):(freeList m)),
                             allocated = al}

freeAll :: Memory -> [Address] -> Memory 
freeAll m [] = m
freeAll m (a:as) = freeAll (free m a) as


compress = merge . List.sort 

merge [] = [] 
merge [x] = [x]
merge ((x,b):(y,b2):xs) = if (x+b == y) then merge ((x,b+b2):xs) 
                           else (x,b):merge((y,b2):xs)



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

livenessSyncUnit aliveNext (SyncUnit nt prgs _) = 
  SyncUnit nt prgs alive
  where alive = foldr Set.union Set.empty (livenessPrograms aliveNext prgs)
  
livenessPrograms aliveNext = map (livenessProgram aliveNext) 

livenessProgram aliveNext (Assign name ix e)  = livingArrs
  where 
    arrays = collectArrays e
    tmp    = Set.fromList arrays `Set.union` aliveNext
    livingArrs = name `Set.delete` tmp
livenessProgram aliveNext (ForAll f n) = livenessProgram aliveNext (f (variable "X"))    
livenessProgram aliveNext (Allocate name size t prg) = livenessProgram aliveNext prg
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
     
    
    
mapMemorySyncUnit (SyncUnit nt ps e) m mm  = mapMemoryPrograms ps m mm 

mapMemoryPrograms [] m mm = (m,mm) 
mapMemoryPrograms (p:ps) m mm = mapMemoryPrograms ps m' mm'
  where 
    (m',mm') = mapMemoryProgram p m mm
    
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

