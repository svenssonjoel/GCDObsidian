{-# LANGUAGE FlexibleContexts #-} 

module Examples where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.C as C
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import Obsidian.GCDObsidian.Program

import Data.Word
import Data.Bits


import Prelude hiding (zipWith,sum )


---------------------------------------------------------------------------
-- MapFusion example
mapFusion :: Array Pull IntE -> Kernel (Array Pull IntE) 
mapFusion = pure (fmap (+1) . fmap (*2)) 

input1 :: Array Pull IntE 
input1 = namedArray "apa" 32

getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
getMapFusionC  = putStrLn$ C.genKernel "mapFusion" mapFusion input1
getMapFusionCL = putStrLn$ CL.genKernel "mapFusion" mapFusion input1


---------------------------------------------------------------------------
-- mapUnfused example
mapUnFused :: Array Pull IntE -> Kernel (Array Pull IntE) 
mapUnFused = pure (fmap (*2)) ->- sync ->- pure (fmap (+1))

getMapUnFused = putStrLn$ CUDA.genKernel "mapUnFused" mapUnFused input1


---------------------------------------------------------------------------
-- reduction of array of length a power of two
reduce :: Syncable (Array Pull) a => (a -> a -> a) -> Array Pull a -> Kernel (Array Pull a)
reduce op arr | len arr == 1 = return arr
              | otherwise    = 
                (pure ((uncurry (zipWith op)) . halve)
                 ->- sync
                 ->- reduce op) arr


reduceS :: (a -> a -> a) -> Array Pull a -> Kernel (Array Pull a) 
reduceS op arr | len arr == 1 = return arr
               | otherwise    = 
                 (pure ((uncurry (zipWith op)) . halve)
                  ->- reduceS op) arr


input8 :: Array Pull IntE 
input8 = namedArray "input" 8

input16 :: Array Pull IntE 
input16 = namedArray "input" 16


getReduceAdd = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input8
getReduceAddLarge = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input256
getReduceAddLargeC = putStrLn$ C.genKernel "reduceAdd" (reduce (+)) input256
getReduceAddLargeCL = putStrLn$ CL.genKernel "reduceAdd" (reduce (+)) input256

getReduceAddC = putStrLn$ C.genKernel "reduceAdd" (reduce (+)) input8                
getReduceSAdd = putStrLn$ CUDA.genKernel "reduceSAdd" (reduceS (+)) input8


catArrays :: (Array Pull (Exp Int), Array Pull (Exp Int)) 
           -> Kernel (Array Pull (Exp Int))
catArrays  = pure conc

getCatArrays = putStrLn$ CUDA.genKernel "catArrays" (catArrays) (input16,input16)


zippUnpair :: (Array Pull IntE, Array Pull IntE) -> Kernel (Array Pull IntE) 
zippUnpair = pure (unpair . zipp)

input32 :: Array Pull IntE 
input32 = namedArray "apa" 32

input64 :: Array Pull IntE 
input64 = namedArray "apa" 64

input128 :: Array Pull IntE
input128 = namedArray "apa" 128


input256 :: Array Pull IntE
input256 = namedArray "apa" 256


getZippUnpair = putStrLn$ CUDA.genKernel "zippUnpair" zippUnpair (input32,input32)


zippUnpairP :: (Array Pull IntE, Array Pull IntE) -> Kernel (Array Push IntE) 
zippUnpairP = pure (unpairP . zipp)

getZippUnpairP = putStrLn$ CUDA.genKernel "zippUnpairP" zippUnpairP (input32,input32)


catArrayPs :: (Array Pull (Exp Int), Array Pull (Exp Int)) 
           -> Kernel (Array Push (Exp Int))
catArrayPs = pure concP -- (arr1,arr2) = return$ concP (arr1, arr2)


getCatArrayPs = putStrLn$ CUDA.genKernel "catArrayPs" (catArrayPs) (input16,input16)





sum :: Array Pull IntE -> Kernel (Array Pull IntE) 
sum arr | len arr == 1 = return arr
        | otherwise    = (pure (fmap (uncurry (+)) . pair) 
                          ->- sync 
                          ->- sum) arr
                         
getSum = putStrLn$ CUDA.genKernel "sum" sum input8                   
getSumIM = snd $ runKernel (sum input8)



---------------------------------------------------------------------------- 
--  GLOBAL ARRAY TESTS 

test1 :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
test1 = pure (block 256) ->- pure (fst . halve) ->- 
        pure (unblock . push) 

getTest1 = putStrLn$ CUDA.genKernelGlob "test1" test1 (GlobalArray undefined (variable "n"):: GlobalArray Pull (Exp Int)) 


testParam1 :: (GlobalArray Pull (Exp Int), Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
testParam1 (garr, param) = res
  where 
    res = test1$ fmap (+param) garr 

getTestParam1_ = putStrLn$ CUDA.genKernelGlob_ "testParam1" testParam1 (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "v") 
getTestParam1 = putStrLn$ CUDA.genKernelGlob "testParam1" testParam1 (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "v") 


globRev (GlobalArray (Pull ixf) n) = GlobalArray (Pull (\ix -> ixf (n - 1 - ix))) n

testGlobRev = pure globRev ->- 
              pure (block 256) ->- 
              pure rev ->- -- also reverse each block 
              pure (unblock . push) 

getTestGlobRev = putStrLn$ CUDA.genKernelGlob "testGlobRev" testGlobRev (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int)) 
getTestGlobRev_ = putStrLn$ CUDA.genKernelGlob_ "testGlobRev" testGlobRev (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int)) 


vSwap :: (GlobalArray Pull (Exp Int), Exp Word32) -> 
         Kernel (GlobalArray Push (Exp Int)) 
vSwap (arr,stride) = return p5
    
  where 
    t1 ix = ix + (ix .&. (complement (stride - 1)))
    t2 ix = (t1 ix) `xor` ((stride `shiftL` 1)-1)
    arr1  = mkGlobalPullArray (\ix -> arr ! t1 ix) (globLen arr `div` 2)
    arr2  = mkGlobalPullArray (\ix -> arr ! t2 ix) (globLen arr `div` 2)
    arr1' = zipWithG min arr1 arr2
    arr2' = zipWithG max arr1 arr2
    p1    = pushGlobal 512 arr1'
    p2    = pushGlobal 512 arr2'
    p3    = ixMap t1 p1 
    p4    = ixMap t2 p2 
    p5    = GlobalArray (Push (\k -> p3 !* k *>* p4 !* k)) (globLen arr)
    
                                     
vSwap' :: (GlobalArray Pull (Exp Int), Exp Word32) -> 
         Kernel (GlobalArray Push (Exp Int)) 
vSwap' (arr,stride) = 
  do 
    p1 <- pushGlobal' 512 arr1'
    p2 <- pushGlobal' 512 arr2'
    let p3    = ixMap t1 p1 
    let p4    = ixMap t2 p2 
    let p5    = GlobalArray (Push (\k -> p3 !* k *>* p4 !* k)) (globLen arr)
    return p5 
    
  where 
    t1 ix = ix + (ix .&. (complement (stride - 1)))
    t2 ix = (t1 ix) `xor` ((stride `shiftL` 1)-1)
    arr1  = mkGlobalPullArray (\ix -> arr ! t1 ix) (globLen arr `div` 2)
    arr2  = mkGlobalPullArray (\ix -> arr ! t2 ix) (globLen arr `div` 2)
    arr1' = zipWithG min arr1 arr2
    arr2' = zipWithG max arr1 arr2
    -- p1    = pushGlobal 512 arr1'
    -- p2    = pushGlobal 512 arr2'
    -- p3    = ixMap t1 p1 
    -- p4    = ixMap t2 p2 
    -- p5    = GlobalArray (Push (\k -> p3 !* k *>* p4 !* k)) (globLen arr)


zipWithG op a1 a2 =  
  mkGlobalPullArray (\ix -> (a1 ! ix) `op` (a2 ! ix))
                   (min (globLen a1) (globLen a2))

-- a global array is "pushed" by dividing 
-- it up into short pieces that are pushed blockwise. 
pushGlobal blocksize = 
   unblock . push . block blocksize   
  
pushGlobal' blocksize = 
  pure  (block blocksize) ->- sync  ->- pure (unblock . push) 
     
getvSwap = putStrLn$ CUDA.genKernelGlob "vSwap" vSwap (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     
getvSwap_ = putStrLn$ CUDA.genKernelGlob_ "vSwap" vSwap (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     

getvSwap' = putStrLn$ CUDA.genKernelGlob "vSwap" vSwap' (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     
getvSwap'_ = putStrLn$ CUDA.genKernelGlob_ "vSwap" vSwap' (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     


----------------------------------------------------------------------------
--
reduceAddBlocks :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
reduceAddBlocks  = withBlockSize 64 (reduce (+)) 

withBlockSize n p = pure (block n) ->- p ->- pure (unblock . push) 

getR = putStrLn$ CUDA.genKernelGlob "reduce" reduceAddBlocks (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     
getR_ = putStrLn$ CUDA.genKernelGlob_ "reduce" reduceAddBlocks (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int))     
