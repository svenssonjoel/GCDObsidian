
module TestP where 

import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Printing

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.C as C
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL



----------------------------------------------------------------------------
--  

small1 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small1 (arr1,arr2) = pSyncArrayP part
  where
    part = concP arr1' arr2'  
    arr1' = push arr1
    arr2' = push arr2 


showSmall1 = printCode$ snd$ runKernel (small1 (namedArray "apa" 32,namedArray "apa" 32))

getSmall1 = putStrLn$ CUDA.genKernel "small1" small1 (namedArray "apa" 32,namedArray "apa" 32)
getSmall1C = putStrLn$ C.genKernel "small1" small1 (namedArray "apa" 32,namedArray "apa" 32)
getSmall1CL = putStrLn$ CL.genKernel "small1" small1 (namedArray "apa" 32,namedArray "apa" 32)  

small2 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small2 (arr1,arr2) = pSyncArray part
  where
    part = conc (arr1,arr2)  

getSmall2 = putStrLn$ CUDA.genKernel "small2" small2 (namedArray "apa" 32,namedArray "apa" 32)
getSmall2C = putStrLn$ C.genKernel "small2" small2 (namedArray "apa" 32,namedArray "apa" 32)
getSmall2CL = putStrLn$ CL.genKernel "small2" small2 (namedArray "apa" 32,namedArray "apa" 32)

    
small3 :: Array (Data Int) -> Kernel (Array (Data Int)) 
small3 arr = pSyncArrayP b 
  where 
    a = rev arr 
    b = revP (push a) 
    
getSmall3 = putStrLn$ CUDA.genKernel "small3" small3 (namedArray "apa" 32)
getSmall3C = putStrLn$ C.genKernel "small3" small3 (namedArray "apa" 32)
getSmall3CL = putStrLn$ CL.genKernel "small3" small3 (namedArray "apa" 32)

    
small4 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int),Array (Data Int)) 
small4 (a1,a2) = pSyncArrays (a1,a2) 

-- DONE: Everything breaks down for this example.
--       Figure it out!
getSmall4 = putStrLn$ CUDA.genKernel "small4" small4 (namedArray "apa" 32, namedArray "apa" 16)
getSmall4C = putStrLn$ C.genKernel "small4" small4 (namedArray "apa" 32, namedArray "apa" 16)
getSmall4CL = putStrLn$ CL.genKernel "small4" small4 (namedArray "apa" 32, namedArray "apa" 16)


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
getSmall5C = putStrLn$ C.genKernel "small5" small5 (namedArray "apa" 32)
getSmall5CL = putStrLn$ CL.genKernel "small5" small5 (namedArray "apa" 32)


small6 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small6 (a1,a2) = 
  do 
    a1' <- pSyncArray a1
    a2' <- pSyncArray a2
    pSyncArray  (conc (a1',a2')) -- length is (len a1 + len a2) here 
    
    
-- perform liveness analysis on small5
runSmall6 = liveness$ snd$ runKernel (small6 (namedArray "apa" 32,namedArray "apa" 32) )
                                       
getSmall6 = putStrLn$ CUDA.genKernel "small6" small6 (namedArray "apa" 32, namedArray "apa" 32)
getSmall6C = putStrLn$ C.genKernel "small6" small6 (namedArray "apa" 32, namedArray "apa" 32)
getSmall6CL = putStrLn$ CL.genKernel "small6" small6 (namedArray "apa" 32, namedArray "apa" 32)


-- DONE: small7 displays a bug. The generated should have an if statement 
--       for the writing of the a2' when a2' has a length shorter that a1.
--       The same if it is a1' that is shorter than a2'
-- DONE: This example also shows a bug in result storing. 
--       This is again a problem in InOut.hs
small7 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small7 (a1,a2) = 
  do 
    let a1' = push a1
    let a2' = push a2
    pSyncArrayP  (concP a1' a2') -- error if (len a1 /= len a2)
    
    
-- perform liveness analysis on small5
    
inputSmall7 = (namedArray "apa" 32,namedArray "apa" 16)

runSmall7 = liveness$ snd$ runKernel (small7 inputSmall7 )

getSmall7 = putStrLn$ CUDA.genKernel "small7" small7 inputSmall7
getSmall7C = putStrLn$ C.genKernel "small7" small7 inputSmall7
getSmall7CL = putStrLn$ CL.genKernel "small7" small7 inputSmall7



-- TODO: This agains shows that writeOutputs is messed up.
small8 :: (Array (Data Int),Array (Data Int)) -> Kernel (Array (Data Int))
small8 (a1,a2) = 
  do 
    let a1' = push a1
    let a2' = push a2
    pSyncArrayP$  concP (concP a1' a2')  a1' 
    
    
-- perform liveness analysis on small5
    
inputSmall8 = (namedArray "apa" 32,namedArray "apa" 16)

runSmall8 = liveness$ snd$ runKernel (small8 inputSmall8 )

getSmall8 = putStrLn$ CUDA.genKernel "small8" small8 inputSmall8
getSmall8C = putStrLn$ C.genKernel "small8" small8 inputSmall8
getSmall8CL = putStrLn$ CL.genKernel "small8" small8 inputSmall8


