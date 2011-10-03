module Obsidian.GCDObsidian.Memory 
       (MemMap,
        Memory,
        allocate,
        free,
        freeAll,
        size, 
        sharedMem,  -- Should be in another Module
        Address,
        Bytes) 
       where 

import qualified Data.List as List
import Data.Word

import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs



import qualified Data.Map as Map 

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






