
import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.CodeGen.CUDA

import Prelude hiding (zipWith)

zipWith :: (Exp a -> Exp b -> Exp c) -> Array a -> Array b -> Array c
zipWith op a1 a2 = Array (\ix -> (a1 ! ix) `op` (a2 ! ix)) (len a1)
  
vectorAdd :: (Array Int, Array Int) -> Kernel (Array Int)   
vectorAdd (a,b) = return$ zipWith (+) a b 
  

testSync :: Array Int -> Kernel (Array Int) 
testSync arr = sync arr 

run1 = 
   putStrLn$ genCUDAKernel "sync" testSync (namedArray "apa" 128)