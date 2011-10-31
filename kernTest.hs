
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Sync

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA 

test1 :: Array (Exp Int) -> Kernel (Array (Exp Int))
test1 = pure (fmap (+1))


test2 :: Array (Exp Int) -> Kernel (Array (Exp Int)) 
test2 inp = do 
  a1 <- pSyncArray inp
  a2 <- pSyncArray a1 
  a3 <- pSyncArray a2 
  a4 <- pSyncArray a3
  a5 <- pSyncArray a1
  return a3
  
  
getTest2 = putStrLn$ CUDA.genKernel "test2" test2 (namedArray "apa" 32) 

small1 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small1 (arr1,arr2) = pSyncArrayP part
  where
    part = concP arr1' arr2'  
    arr1' = push arr1
    arr2' = push arr2 

getSmall1 = putStrLn$ CUDA.genKernel "small1" small1 (namedArray "apa" 32,namedArray "apa" 32)

small6 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small6 (a1,a2) = 
  do 
    a1' <- pSyncArray a1
    a2' <- pSyncArray a2
    pSyncArray  (conc (a1',a2')) -- length is (len a1 + len a2) here 
    
getSmall6 = putStrLn$ CUDA.genKernel "small6" small6 (namedArray "apa" 32, namedArray "bepa" 16)
getSmall6' = runKernel$ small6 (namedArray "apa" 32, namedArray "bepa" 16)


small8 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small8 (a1,a2) = 
  do 
    let a1' = push a1
    let a2' = push a2
    pSyncArrayP$  concP (concP a1' a2')  a1' 

inputSmall8 = (namedArray "apa" 32,namedArray "apa" 16)
getSmall8 = putStrLn$ CUDA.genKernel "small8" small8 inputSmall8