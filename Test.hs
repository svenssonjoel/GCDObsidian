
import Obsidian.GCDObsidian
import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Store
import Obsidian.GCDObsidian.Printing
import Obsidian.GCDObsidian.Evaluate

import qualified Obsidian.GCDObsidian.CodeGen.CUDA   as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as OpenCL
import qualified Obsidian.GCDObsidian.CodeGen.C as C 

import Prelude hiding (zipWith,splitAt)
import Data.Word
  
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


testComp :: Array Int -> Kernel (Array Int) 
testComp = (pure rev) ->- sync ->- (pure rev)

run3 = 
  putStrLn$ CUDA.genCUDAKernel "comp" testComp (namedArray "apa" 32)
   
run3CL =    
  putStrLn$ OpenCL.genOpenCLKernel "comp" testComp (namedArray "apa" 32)
  
  
    
testStoreIlv :: (Array Int, Array Int) -> Kernel (Array Int) 
testStoreIlv inputs = (storeIlvF ->- pure rev ->- sync) (zipp inputs)

run4 = 
  putStrLn$ CUDA.genCUDAKernel "storeIlv" testStoreIlv (namedArray "apa" 32,namedArray "apa" 32)
   
run4CL =    
  putStrLn$ OpenCL.genOpenCLKernel "storeIlv" testStoreIlv (namedArray "apa" 32,namedArray "apa" 32)
  


sklansky :: Int -> Array Word32 -> Kernel (Array Word32)
sklansky 0 = pure id 
sklansky n = pure (twoK (n-1) fan) ->- store ->- sklansky (n-1)
  where fan arr = conc (a1,a2')
          where 
            (a1,a2) = splitAt middle arr  
            middle = len arr `div` 2 
            n = len a1          
            e = a1 ! (fromIntegral (n-1)) 
            a2' = (Array (\ix -> (a2 ! ix) + e) (len a2))
                    
            
run5 = 
  putStrLn$ CUDA.genCUDAKernel "sklansky" (sklansky 5) (namedArray "apa" 32)
   
run5CL =    
  putStrLn$ OpenCL.genOpenCLKernel "sklansky" (sklansky 5)(namedArray "apa" 32)
  
  
run5C =    
  putStrLn$ C.genCKernel "sklansky" (sklansky 5)(namedArray "apa" 32)
    
