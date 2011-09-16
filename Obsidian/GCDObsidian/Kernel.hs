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
            -> LLArray a -- Data 
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
-- The GPU program is just a list of stores...  (for now) 
-- Just a list 
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

{- 
livenessWrite :: Liveness -> Write a -> Write Liveness 
livenessWrite aliveNext (Write n ff e) 
  = Write n ff livingArrs 
    where 
      exp = ff ! tid 
      arrays = collectArrays exp 
      tmp    = Set.fromList arrays `Set.union` aliveNext
      livingArrs = rootName (n (variable "X")) `Set.delete` tmp

rootName (Index (name,_)) = name
rootName _ = error "livenessWrite: malformed assignment location"
-} 

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
{-
mapMemory :: Code Liveness -> Memory -> ArraySizes -> MemMap -> (Memory,MemMap)
mapMemory Skip m as mm = (m,mm) 
mapMemory (store `Seq` code) m as mm = mapMemory code m'' as mm' 
  where 
    (m',mm')  = mapMemoryStore store m as mm 
    aliveNext = whatsAliveNext code 
    aliveNow  = getExtraStore store Set.union
    diff      = aliveNow Set.\\ aliveNext
    diffAddr  = mapM (\x -> Map.lookup x mm') (filter (not . (isPrefixOf "input")) (Set.toList diff))
    m''       = case diffAddr of 
                   (Just addys) -> freeAll m' (map fst addys)
                   Nothing      -> error "atleast one array does not exist in memorymap" 
-}  
     
mapMemoryStoreList StoreListNil m mm = (m,mm)
mapMemoryStoreList (StoreListCons s rest) m mm = mapMemoryStoreList rest m' mm' 
  where
    (m',mm') = mapMemoryStore s m mm 

mapMemoryStore :: Scalar a => Store a Liveness -> Memory -> MemMap -> (Memory,MemMap)
mapMemoryStore (Store name size ws) m  mm = (m',mm')  --allocateWrites ws m as mm 
  where 
    (m'',addr) = allocate m size
    t = Pointer$ typeOf$ getLLArray (head ws) `llIndex`  tid
    (m',mm') = 
      case Map.lookup name mm of 
        Nothing      -> (m'',Map.insert name (addr,t) mm)
        (Just (a,t)) -> (m,mm) -- what does this case really mean ? -
{-
allocateWrites :: [Write extra] -> Memory -> ArraySizes -> MemMap -> (Memory,MemMap)
allocateWrites [] m as mm = (m,mm) 
allocateWrites ((Write n ll _):ws) m as mm = allocateWrites ws m' as mm'
  where 
    -- TODO: This is still wrong. (the array sizes will be wrong) 
    ((m',addr),mm') = case Map.lookup name mm of 
       Nothing -> 
         case Map.lookup name as of 
           Nothing -> error "bad implementation" 
           (Just b) -> (allocate m (fromIntegral b),Map.insert (rootName (n (variable "X"))) (addr,t) mm)
       (Just (a,t)) -> ((m,a),mm)
    name = rootName (n (variable "X"))
    t = Pointer (typeOf (ll ! tid)) -- in the write "object" 
-}
    


------------------------------------------------------------------------------
-- numbers of threads needed to compute a kernel
threadsNeeded :: Code a -> Word32
threadsNeeded Skip = 0
threadsNeeded ((SyncUnit nt _)  `Seq` c2) = nt `max` threadsNeeded c2 

