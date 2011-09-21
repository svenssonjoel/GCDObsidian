
module TestP where 

import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Printing

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA


----------------------------------------------------------------------------
--  

small1 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small1 (arr1,arr2) = pSyncArrayP part
  where
    part = concP arr1' arr2'  
    arr1' = toArrayP arr1
    arr2' = toArrayP arr2 


showSmall1 = printCode$ snd$ runKernel (small1 (namedArray "apa" 32,namedArray "apa" 32))

getSmall1 = putStrLn$ CUDA.genKernel "small1" small1 (namedArray "apa" 32,namedArray "apa" 32)
  

small2 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small2 (arr1,arr2) = pSyncArray part
  where
    part = conc (arr1,arr2)  

getSmall2 = putStrLn$ CUDA.genKernel "small2" small2 (namedArray "apa" 32,namedArray "apa" 32)

    
small3 :: Array (Data Int) -> Kernel (Array (Data Int)) 
small3 arr = pSyncArrayP b 
  where 
    a = rev arr 
    b = revP (toArrayP a) 
    
getSmall3 = putStrLn$ CUDA.genKernel "small3" small3 (namedArray "apa" 32)

    
small4 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int),Array (Data Int)) 
small4 (a1,a2) = pSyncArrays (a1,a2) 

-- TODO: Everything breaks down for this example.
--       Figure it out!
getSmall4 = putStrLn$ CUDA.genKernel "small4" small4 (namedArray "apa" 32, namedArray "apa" 16)


small5 :: Array (Data Int) -> Kernel (Array (Data Int)) 
small5 arr = 
  do 
    a0 <- pSyncArray arr
    a1 <- pSyncArray a0
    a2 <- pSyncArray a1
    a3 <- pSyncArray a2
    a4 <- pSyncArray a0
    return a4
    
    
-- perform liveness analysis on small5
runSmall5 = liveness$ snd$ runKernel (small5 (namedArray "apa" 32) )
                                       
getSmall5 = putStrLn$ CUDA.genKernel "small5" small5 (namedArray "apa" 32)


small6 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small6 (a1,a2) = 
  do 
    a1' <- pSyncArray a1
    a2' <- pSyncArray a2
    pSyncArray  (conc (a1',a2')) -- length is (len a1 + len a2) here 
    
    
-- perform liveness analysis on small5
runSmall6 = liveness$ snd$ runKernel (small6 (namedArray "apa" 32,namedArray "apa" 32) )
                                       
getSmall6 = putStrLn$ CUDA.genKernel "small6" small6 (namedArray "apa" 32, namedArray "apa" 32)
