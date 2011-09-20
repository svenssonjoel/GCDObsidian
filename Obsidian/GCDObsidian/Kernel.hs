{-# LANGUAGE TypeOperators,
             GADTs ,              
             RankNTypes, 
             FlexibleContexts,
             FlexibleInstances #-}

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
-- Kernels!

type NumThreads = Word32 


------------------------------------------------------------------------------
-- Ways to write data into GPU memory
data  Write a extra where  
  Write :: Scalar a
            => (Exp Word32 -> Exp Word32)  -- target location transformation
            -> LLArray a -- array to store
            -> extra 
            -> Write a extra                 
  Permute :: Scalar a 
            => (Exp Word32 -> Exp Word32)    -- target storage
            -> LLArray a   -- Data 
            -> LLArray Int -- Permutation
            -> extra 
            -> Write a extra 
            
------------------------------------------------------------------------------            
-- Extract extra information from a write 
getExtra :: Write a extra -> extra 
getExtra (Write _ _  e ) = e 

getLLArray :: Scalar a => Write a extra -> LLArray a 
getLLArray (Write _ ll _) = ll

------------------------------------------------------------------------------
-- Convenience
write :: Scalar a => Name -> LLArray a -> extra -> Write a extra 
write nom ll@(LLArray ixf n m d) e 
  = Write (\ix -> ix) ll e    -- do not transform location

    
------------------------------------------------------------------------------
-- A Store 

data Store a extra = Store {storeName    :: Name,
                            storeSize    :: Word32,
                            storeWrites  :: [Write a extra]}  

data StoreList extra = StoreListNil 
               | forall a. Scalar a => StoreListCons (Store a extra) (StoreList extra) 
                 
storeListConcat :: StoreList e -> StoreList e -> StoreList e                   
storeListConcat (StoreListNil) sl = sl
storeListConcat (StoreListCons s sl) sl' = StoreListCons s (storeListConcat sl sl')


data SyncUnit extra = SyncUnit {syncThreads :: Word32, 
                                syncStores  :: StoreList extra} 

syncUnitFuseGCD :: SyncUnit e -> SyncUnit e -> SyncUnit e 
syncUnitFuseGCD (SyncUnit nt sl) (SyncUnit nt' sl')  = 
    SyncUnit (gcd nt nt') (storeListConcat sl sl')

syncUnitGetExtra :: SyncUnit extra -> (extra -> extra -> extra) -> extra -> extra 
syncUnitGetExtra su f base = storeListGetExtra (syncStores su) f base

-- looks like fold... see if something is possible there
storeListGetExtra :: StoreList extra -> (extra -> extra -> extra) -> extra -> extra 
storeListGetExtra StoreListNil combf base = base
storeListGetExtra (StoreListCons s rest) combf base = 
  getExtraStore s combf  `combf` (storeListGetExtra rest combf base)

------------------------------------------------------------------------------- 
-- Get the extra information from a store 
-- by combining the extra information of each of its writes
getExtraStore :: Store a extra -> (extra -> extra -> extra) ->  extra
getExtraStore store f = foldl1 f (map getExtra (getWrites store)) 

-------------------------------------------------------------------------------
-- Get the writes out from a store
getWrites :: Store a extra -> [Write a extra] 
getWrites = storeWrites    

------------------------------------------------------------------------------
-- The GPU program is just a list of "SyncUnits"...  (for now)  
data Code extra where 
  Skip :: Code extra 
  Seq  :: SyncUnit extra -> Code extra -> Code extra 


(+++) :: Code a -> Code a -> Code a 
Skip +++ a = a
(a `Seq` c1) +++ b = a `Seq` (c1 +++ b)   

------------------------------------------------------------------------------
-- Turn a Store into a Code
code :: SyncUnit extra -> Code extra
code s = Seq s Skip       


-- Needs Monoid instance for writer.. 
instance Monoid (Code a) where 
  mempty = Skip
  mappend a b = a +++ b

------------------------------------------------------------------------------
-- KERNEL
  

type Kernel a = StateT Integer (Writer (Code ())) a   

newArray :: Word32 -> Kernel Name 
newArray nBytes = do
  i <- get
  let newName = "arr" ++ show i 
  put (i+1)
  return newName



type a :-> b = a -> Kernel b     

(->-) :: (a -> Kernel b) -> (b -> Kernel c) -> (a -> Kernel c) 
(->-) = (>=>) 

pure f a = return (f a) 

runKernel k = runWriter (runStateT k 0 )

------------------------------------------------------------------------------  
-- Tid. Probably not really needed here

tid :: Exp Word32
tid = variable "tid"
      
      
------------------------------------------------------------------------------
-- Code analysis .... 

type Liveness = Set.Set String

liveness :: Code a -> Code Liveness 
liveness (s `Seq` c) = lives `Seq` livec 
    where 
      lives = livenessSyncUnit aliveNext s  
      livec = liveness c
      aliveNext = whatsAliveNext livec

liveness Skip = Skip 

livenessSyncUnit aliveNext (SyncUnit nt stores) = 
  SyncUnit nt (livenessStoreList aliveNext stores)

livenessStoreList :: Liveness -> StoreList extra -> StoreList Liveness 
livenessStoreList _ StoreListNil = StoreListNil
livenessStoreList aliveNext (StoreListCons s rest) = 
  StoreListCons (livenessStore aliveNext s) 
    (livenessStoreList aliveNext rest)

livenessStore :: Scalar a => Liveness -> Store a extra -> Store a Liveness
livenessStore aliveNext (Store name size ws) = 
    Store name size (map (addLiveness livingArrs {-aliveNext-}) ws) -- HACK 
    where 
      arrays = concatMap (collectArrays . ((flip llIndex) tid) . getLLArray ) ws
      tmp    = Set.fromList arrays `Set.union` aliveNext 
      livingArrs = name `Set.delete` tmp

-- HACK 
addLiveness :: Liveness -> Write a extra -> Write a Liveness
addLiveness live (Write targf ll _) = Write targf ll live 


whatsAliveNext :: Code Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (s `Seq` _) =  syncUnitGetExtra s Set.union Set.empty
    -- map getExtra$  getWrites s
  

------------------------------------------------------------------------------
-- Memory layout

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
-- Create a memory map

type MemMap = Map.Map Name (Address,Type) 

-- TODO: if strange behaviour look here first !! 
mapMemory :: Code Liveness -> Memory -> MemMap -> (Memory,MemMap) 
mapMemory Skip m mm = (m,mm) 
mapMemory (su `Seq` code) m mm = mapMemory code m' mm' 
  where 
    (m'',mm') = mapMemoryStoreList (syncStores su) m mm 
    aliveNext = whatsAliveNext code
    aliveNow  = syncUnitGetExtra su Set.union Set.empty
    diff      = aliveNow Set.\\ aliveNext
    diffAddr  = mapM (\x -> Map.lookup x mm') (filter (not . (isPrefixOf "input")) (Set.toList diff))
    m'        = 
      case diffAddr of 
        (Just addys) -> freeAll m'' (map fst addys)
        Nothing      -> error "atleast one array does not exist in memorymap" 
     
mapMemoryStoreList StoreListNil m mm = (m,mm)
mapMemoryStoreList (StoreListCons s rest) m mm = mapMemoryStoreList rest m' mm' 
  where
    (m',mm') = mapMemoryStore s m mm 

mapMemoryStore :: Scalar a => Store a Liveness -> Memory -> MemMap -> (Memory,MemMap)
mapMemoryStore (Store name size ws) m  mm = (m',mm')  --allocateWrites ws m as mm 
  where 
    (m'',addr) = allocate m size
    -- TODO: maybe create Global arrays if Local memory is full.
    t = Pointer$ Local$ typeOf$ getLLArray (head ws) `llIndex`  tid
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -


------------------------------------------------------------------------------
-- numbers of threads needed to compute a kernel
threadsNeeded :: Code a -> Word32
threadsNeeded Skip = 0
threadsNeeded ((SyncUnit nt _)  `Seq` c2) = nt `max` threadsNeeded c2 







------------------------------------------------------------------------------
-- Pushy Kernels


data PSyncUnit extra = PSyncUnit {pSyncThreads :: Word32, 
                                  pSyncPrograms :: [Program],
                                  pSyncExtra    :: extra }
                 deriving Show
                          
pSyncUnit :: Word32 -> [Program] -> PSyncUnit ()                          
pSyncUnit w ps = PSyncUnit w ps ()


data PCode extra = PSkip -- nothing to do 
                 | (PSyncUnit extra) `PSeq` (PCode extra) 
                 deriving Show                    
                   
instance Monoid (PCode extra) where                    
  mempty = PSkip
  mappend PSkip a = a 
  mappend a PSkip = a 
  mappend (ps `PSeq` c) c2 = ps `PSeq` (mappend c c2)
                   
type PKernel a = StateT Integer (Writer (PCode ())) a   

runPKernel k = runWriter (runStateT k 0)

newArrayP :: PKernel Name 
newArrayP  = do
  i <- get
  let newName = "arr" ++ show i 
  put (i+1)
  return newName


(->>-) :: (a -> PKernel b) -> (b -> PKernel c) -> (a -> PKernel c) 
(->>-) = (>=>) 


------------------------------------------------------------------------------
threadsNeededP :: PCode e -> Word32
threadsNeededP PSkip = 0
threadsNeededP (PSeq (PSyncUnit nt _ _) c) = 
  max nt (threadsNeededP c)

threadsNeededPSU :: Program -> Word32 
threadsNeededPSU (Assign name n a) = 1
threadsNeededPSU (ForAll f n) = n  
threadsNeededPSU (psu1 `ProgramSeq` psu2) = max (threadsNeededPSU psu1) 
                                          (threadsNeededPSU psu2)



------------------------------------------------------------------------------
-- Syncs in the new setting 

-- Work on the Scalar a thing!!!
pSyncArray  :: Scalar a => Array (Exp a) -> PKernel (Array (Exp a))
pSyncArray arr = 
  do 
    name <- newArrayP 
    
    tell$ PSeq (pSyncUnit (len arr) 
                [Allocate name (es * (len arr)) t 
                  (pushApp parr (targetArray name))]) PSkip
            
    return (Array (index name) (len arr))
      
  where 
    es = fromIntegral$ sizeOf (arr ! 0) 
    t  = Pointer$ Local$ typeOf (arr ! 0)

    parr = toArrayP arr 
        

-- THE GCD THING
pSyncArrays :: (Scalar a, Scalar b) => (Array (Exp a),Array (Exp b)) -> PKernel (Array (Exp a), Array (Exp b))
pSyncArrays (a1,a2) = 
  do 
    name1 <- newArrayP 
    name2 <- newArrayP 
    
    tell$ PSeq (pSyncUnit n [Allocate name1 (es1 * (len a1)) t1
                             (pushApp pa1 (targetArray name1))
                            ,Allocate name2 (es2 * (len a2)) t2
                             (pushApp pa2 (targetArray name2))]) PSkip
            
    return (Array (index name1) (len a1)
           ,Array (index name2) (len a2))
      
  where  
    es1 = fromIntegral$ sizeOf (a1 ! 0) 
    es2 = fromIntegral$ sizeOf (a2 ! 0) 
    t1  = Pointer$ Local$ typeOf (a1 ! 0)
    t2  = Pointer$ Local$ typeOf (a2 ! 0)
    n   = gcd (len a1) (len a2) 
    pa1 = toArrayP a1 
    pa2 = toArrayP a2    

    
pSyncArrayP :: Scalar a => ArrayP (Exp a) -> PKernel (Array (Exp a)) 
pSyncArrayP arr@(ArrayP func n)  = 
  do 
    name <- newArrayP 
    
    let result = Array (index name) n         
        es = fromIntegral$ sizeOf (result ! 0) 
        t  = Pointer$ Local$ typeOf (result ! 0)


    tell$ PSeq (pSyncUnit n 
                [Allocate name (es * n) t 
                 (pushApp arr (targetArray name))]) PSkip
    return result

------------------------------------------------------------------------------
-- LIVENESS on PCODE 


livenessP :: PCode a -> PCode Liveness 
livenessP (s `PSeq` c) = lives `PSeq` livec 
    where 
      lives = livenessPSyncUnit aliveNext s  
      livec = livenessP c
      aliveNext = whatsAliveNextP livec

livenessP PSkip = PSkip 

livenessPSyncUnit aliveNext (PSyncUnit nt prgs _) = 
  PSyncUnit nt prgs alive
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



whatsAliveNextP :: PCode Liveness -> Liveness
whatsAliveNextP PSkip = Set.empty
whatsAliveNextP (s `PSeq` _) = pSyncExtra s

------------------------------------------------------------------------------ 




------------------------------------------------------------------------------
-- Create a memory map on PCODE 


mapMemoryP :: PCode Liveness -> Memory -> MemMap -> (Memory,MemMap) 
mapMemoryP PSkip m mm = (m,mm) 
mapMemoryP (su `PSeq` code) m mm = mapMemoryP code m' mm' 
  where 
    (m'',mm') = mapMemoryPSyncUnit su m mm 
    aliveNext = whatsAliveNextP code
    aliveNow  = pSyncExtra su 
    diff      = aliveNow Set.\\ aliveNext
    diffAddr  = mapM (\x -> Map.lookup x mm') (filter (not . (isPrefixOf "input")) (Set.toList diff))
    m'        = 
      case diffAddr of 
        (Just addys) -> freeAll m'' (map fst addys)
        Nothing      -> error "atleast one array does not exist in memorymap" 
     
    
    
mapMemoryPSyncUnit (PSyncUnit nt ps e) m mm  = mapMemoryPrograms ps m mm 

mapMemoryPrograms [] m mm = (m,mm) 
mapMemoryPrograms (p:ps) m mm = mapMemoryPrograms ps m' mm'
  where 
    (m',mm') = mapMemoryProgram p m mm
    
mapMemoryProgram (Assign name i a) m mm = (m,mm) 
mapMemoryProgram (ForAll f n) m mm = mapMemoryProgram (f (variable "X")) m mm       
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


{-       
mapMemoryStoreList StoreListNil m mm = (m,mm)
mapMemoryStoreList (StoreListCons s rest) m mm = mapMemoryStoreList rest m' mm' 
  where
    (m',mm') = mapMemoryStore s m mm 

mapMemoryStore :: Scalar a => Store a Liveness -> Memory -> MemMap -> (Memory,MemMap)
mapMemoryStore (Store name size ws) m  mm = (m',mm')  --allocateWrites ws m as mm 
  where 
    (m'',addr) = allocate m size
    -- TODO: maybe create Global arrays if Local memory is full.
    t = Pointer$ Local$ typeOf$ getLLArray (head ws) `llIndex`  tid
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -


-}