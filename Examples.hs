{-# LANGUAGE FlexibleContexts,  
             TypeFamilies#-}

module Examples where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA


import Prelude hiding (zipWith)


mapFusion :: Array IntE -> Kernel (Array IntE) 
mapFusion = pure (fmap (+1) . fmap (*2)) 

input1 :: Array IntE 
input1 = namedArray "apa" 32

getMapFusion = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1





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
input8 = namedArray "apa" 8


getReduceAdd = putStrLn$ CUDA.genKernel "reduceAdd" (reduce (+)) input8
getReduceSAdd = putStrLn$ CUDA.genKernel "reduceSAdd" (reduceS (+)) input8
