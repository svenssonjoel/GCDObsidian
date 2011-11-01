{-# LANGUAGE FlexibleContexts,  
             TypeFamilies#-}

module Examples where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.C as C
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL


import Prelude hiding (zipWith,sum )


--mapFusion :: Array IntE -> Kernel (Array IntE) 
--mapFusion = pure (fmap (*2)) ->- pure (fmap (+1)) 



mapFusion :: Array IntE -> Kernel (Array IntE) 
mapFusion = pure (fmap (+1) . fmap (*2)) 

input1 :: Array IntE 
input1 = namedArray "apa" 32

getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
getMapFusionC  = putStrLn$ C.genKernel "mapFusion" mapFusion input1
getMapFusionCL = putStrLn$ CL.genKernel "mapFusion" mapFusion input1


mapUnFused :: Array IntE -> Kernel (Array IntE) 
mapUnFused = pure (fmap (*2)) ->- sync ->- pure (fmap (+1))

getMapUnFused = putStrLn$ CUDA.genKernel "mapUnFused" mapUnFused input1


-- reduce a power of two length array 
reduce :: (Syncable' (Array a), 
          Synced (Array a) ~ (Array a)) => (a -> a -> a) -> Array a -> Kernel (Array a)
reduce op arr | len arr == 1 = return arr
              | otherwise    = 
                (pure ((uncurry (zipWith op)) . halve)
                 ->- sync' 
                 ->- reduce op) arr


reduceS :: (a -> a -> a) -> Array a -> Kernel (Array a) 
reduceS op arr | len arr == 1 = return arr
               | otherwise    = 
                 (pure ((uncurry (zipWith op)) . halve)
                  ->- reduceS op) arr


input8 :: Array IntE 
input8 = namedArray "input" 8

input16 :: Array IntE 
input16 = namedArray "input" 16


getReduceAdd = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input8
getReduceAddLarge = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input256
getReduceAddC = putStrLn$ C.genKernel "reduceAdd" (reduce (+)) input8                

getReduceSAdd = putStrLn$ CUDA.genKernel "reduceSAdd" (reduceS (+)) input8



catArrays :: (Array (Exp Int), Array (Exp Int)) 
           -> Kernel (Array (Exp Int))
catArrays  = pure conc 

getCatArrays = putStrLn$ CUDA.genKernel "catArrays" (catArrays) (input16,input16)


zippUnpair :: (Array IntE, Array IntE) -> Kernel (Array IntE) 
zippUnpair = pure (unpair . zipp)

input32 :: Array IntE 
input32 = namedArray "apa" 32

input64 :: Array IntE 
input64 = namedArray "apa" 64

input128 :: Array IntE
input128 = namedArray "apa" 128


input256 :: Array IntE
input256 = namedArray "apa" 256


getZippUnpair = putStrLn$ CUDA.genKernel "zippUnpair" zippUnpair (input32,input32)



zippUnpairP :: (Array IntE, Array IntE) -> Kernel (ArrayP IntE) 
zippUnpairP = pure (unpairP . zipp)

getZippUnpairP = putStrLn$ CUDA.genKernel "zippUnpairP" zippUnpairP (input32,input32)


catArrayPs :: (Array (Exp Int), Array (Exp Int)) 
           -> Kernel (ArrayP (Exp Int))
catArrayPs = pure concP -- (arr1,arr2) = return$ concP (arr1, arr2)

getCatArrayPs = putStrLn$ CUDA.genKernel "catArrayPs" (catArrayPs) (input32,input32)





sum :: Array IntE -> Kernel (Array IntE) 
sum arr | len arr == 1 = return arr
        | otherwise    = (pure (fmap (uncurry (+)) . pair) 
                          ->- sync 
                          ->- sum) arr
                         
getSum = putStrLn$ CUDA.genKernel "sum" sum input8                   
getSumIM = snd $ runKernel (sum input8)



-- SyncAnalysis seems to work for extremely simple cases. 
-- TODO: need more involved programs to test on. 
testSA arr = 
  do 
    arr1 <- sync arr 
    arr2 <- sync arr1 
    arr3 <- sync arr2
    arr4 <- sync arr3 
    arr5 <- sync arr4 
    arr6 <- (pure (twoK 1 rev) ->- sync) arr5
    return arr6
    
getTestSA  = putStrLn$ CUDA.genKernel "sa" testSA input64

testSA1 arr = 
  do 
    arr1 <- sync arr 
    arr2 <- sync arr1 
    arr3 <- sync arr2
    arr4 <- sync arr3 
    arr5 <- sync (rev arr4)
    arr6 <- (pure (twoK 1 rev) ->- sync) arr5
    return arr6
    
getTestSA1  = putStrLn$ CUDA.genKernel "sa" testSA1 input128


testSA2 arr = 
  do 
    arr1 <- sync arr 
    arr2 <- sync arr1 
    arr3 <- sync arr2
    arr4 <- sync arr3 
    arr5 <- sync (rev arr4)
    arr6 <- (pure (twoK 2 rev) ->- sync) arr5
    return arr6
    
getTestSA2  = putStrLn$ CUDA.genKernel "sa" testSA2 input256