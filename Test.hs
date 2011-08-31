
import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA   as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as OpenCL


import Prelude hiding (zipWith)

zipWith :: (Exp a -> Exp b -> Exp c) -> Array a -> Array b -> Array c
zipWith op a1 a2 = Array (\ix -> (a1 ! ix) `op` (a2 ! ix)) (len a1)
  
vectorAdd :: (Array Int, Array Int) -> Kernel (Array Int)   
vectorAdd (a,b) = return$ zipWith (+) a b 
  

testSync :: Array Int -> Kernel (Array Int) 
testSync arr = sync arr 

run1 =    
  putStrLn$ CUDA.genCUDAKernel "sync" testSync (namedArray "apa" 128)
   
run1CL =   
  putStrLn$ OpenCL.genOpenCLKernel "sync" testSync (namedArray "apa" 128)
  
testTwo :: Array Int -> Kernel (Array Int) 
testTwo arr = do 
  arr1 <- return$ twoK 1 rev arr  
  sync arr1 -- and a sync for fun

run2 = 
   putStrLn$ CUDA.genCUDAKernel "two" testTwo (namedArray "apa" 32)
   
run2CL =    
     putStrLn$ OpenCL.genOpenCLKernel "two" testTwo (namedArray "apa" 32)
