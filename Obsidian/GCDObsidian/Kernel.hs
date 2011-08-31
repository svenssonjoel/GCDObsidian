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
data  Write extra where  
  Write :: forall a extra . Scalar a 
            => (Exp Word32 -> Exp a) -- Name  -- target storage
            -> LLArray a -- array to store
            -> extra 
            -> Write extra 
  Permute :: forall a extra . Scalar a 
            => (Exp Word32 -> Exp a)    -- target storage
            -> LLArray a -- Data 
            -> LLArray Int -- Permutation
            -> extra 
            -> Write extra 
            
------------------------------------------------------------------------------            
-- Extract extra information from a write 
getExtra :: Write extra -> extra 
getExtra (Write _ _  e ) = e 


------------------------------------------------------------------------------
-- Convenience
write :: Scalar a => Name -> LLArray a -> extra -> Write extra 
write nom ll@(LLArray ixf n m d) e 
  = Write (index nom) ll e 

    
------------------------------------------------------------------------------
-- A Store ... (Is this really needed ?) 
data Store extra = Store NumThreads [Write extra]  

------------------------------------------------------------------------------- 
-- Get the extra information from a store 
-- by combining the extra information of each of its writes
getExtraStore :: Store extra -> (extra -> extra -> extra) ->  extra
getExtraStore store f = foldl1 f (map getExtra (getWrites store)) 

-------------------------------------------------------------------------------
-- Get the writes out from a store
getWrites :: Store extra -> [Write extra] 
getWrites (Store n ws) = ws    


------------------------------------------------------------------------------
-- The GPU program is just a list of stores...  (for now) 
-- Just a list 
data Code extra where 
  Skip :: Code extra 
  Seq  :: Store extra -> Code extra -> Code extra 


(+++) :: Code a -> Code a -> Code a 
Skip +++ a = a
(a `Seq` c1) +++ b = a `Seq` (c1 +++ b)   

------------------------------------------------------------------------------
-- Turn a Store into a Code
code :: Store a -> Code a 
code s = Seq s Skip       


-- Needs Monoid instance for writer.. 
instance Monoid (Code a) where 
  mempty = Skip
  mappend a b = a +++ b
  

type Kernel a = StateT Integer (Writer (Code ())) a   
type a :-> b = a -> Kernel b     


runKernel k = runWriter (runStateT k 0 )

------------------------------------------------------------------------------  
-- Simple sync

tid :: Exp Word32
tid = variable "tid"

sync :: (OArray a e, Scalar e) => a e -> Kernel (a e)
sync arr = 
  do 
    let ll@(LLArray ixf n m d) = toLL arr 
    
    (w,r) <- writeNT m ll 
    tell$ code$ Store m w 
    -- tell Synchronize
    return$ fromLL r
    
  
sync2 :: (OArray a e, Scalar e, 
          OArray a' e', Scalar e') 
         => (a e,a' e') 
         -> Kernel (a e,a' e') 
sync2 (a1,a2)  = 
  do         
    (w1,r1) <- writeNT numThreads ll1
    (w2,r2) <- writeNT numThreads ll2
    
    tell$ code$ Store numThreads (w1++w2)
                           
    -- tell Synchronize
    -- No do not "tell Synchronize". 
    -- Discover if to Synchronize or not later. 
 
    return$ (fromLL r1,fromLL r2)  
    where 
      ll1 = toLL a1 
      ll2 = toLL a2   
      numThreads = gcd (staticLength ll1) (staticLength ll2)
    
writeNT nt ll@(LLArray ixf n m d) = 
  do 
    i <- get 
    put (i+1) 
    let newName  = "arr" ++ show i
        newArray = LLArray (\ix -> Index (newName,[ix])) n m d 
                
    return ([write newName ll () ],newArray)
                    
  
                             
------------------------------------------------------------------------------
-- Code analysis .... 

type Liveness = Set.Set String

liveness :: Code a -> Code Liveness 
liveness (s `Seq` c) = lives `Seq` livec 
    where 
      lives = livenessStore aliveNext s  
      livec = liveness c
      aliveNext = whatsAliveNext livec

liveness Skip = Skip 

livenessStore :: Liveness -> Store a -> Store Liveness
livenessStore aliveNext (Store nt ws) = 
    Store nt (map (livenessWrite aliveNext) ws) 

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

whatsAliveNext :: Code Liveness -> Liveness
whatsAliveNext Skip = Set.empty
whatsAliveNext (s `Seq` _) = Set.unions$ map getExtra$  getWrites s
  

------------------------------------------------------------------------------
-- Memory layout

type Address = Word64
type Bytes   = Word64 

data Memory = Memory {freeList  :: [(Address,Bytes)] ,
                      allocated :: [(Address,Bytes)] , 
                      size      :: Bytes} 
              
              
sharedMem = Memory [(0,48000)] [] 0

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

mapMemory :: Code Liveness -> Memory -> MemMap -> (Memory,MemMap)
mapMemory Skip m mm = (m,mm) 
mapMemory (store `Seq` code) m mm = mapMemory code m'' mm' 
  where 
    (m',mm')  = mapMemoryStore store m mm 
    aliveNext = whatsAliveNext code 
    aliveNow  = getExtraStore store Set.union
    diff      = aliveNow Set.\\ aliveNext
    diffAddr  = mapM (\x -> Map.lookup x mm') (filter (not . (isPrefixOf "input")) (Set.toList diff))
    m''       = case diffAddr of 
                   (Just addys) -> freeAll m' (map fst addys)
                   Nothing      -> error "atleast one array does not exist in memorymap" 
      
mapMemoryStore :: Store Liveness -> Memory -> MemMap -> (Memory,MemMap)
mapMemoryStore (Store nt ws) m mm = allocateWrites ws m mm 


allocateWrites :: [Write extra] -> Memory -> MemMap -> (Memory,MemMap)
allocateWrites [] m mm = (m,mm) 
allocateWrites ((Write n ll _):ws) m mm = allocateWrites ws m' mm'
  where 
    (m', addr) = allocate m bytesNeeded
    mm'        = Map.insert (rootName (n (variable "X"))) (addr,t) mm 
    
    bytesNeeded = fromIntegral s * fromIntegral (staticLength ll) 
    s = sizeOf (ll ! tid) -- If I am doing this a lot rethink having the array  
    t = Pointer (typeOf (ll ! tid)) -- in the write "object" 
    -- Add a "Pointer" type associated to the "array" into the MM 
    


------------------------------------------------------------------------------
-- numbers of threads needed to compute a kernel
threadsNeeded :: Code a -> Word32
threadsNeeded Skip = 0
threadsNeeded ((Store nt _)  `Seq` c2) = nt `max` threadsNeeded c2 

