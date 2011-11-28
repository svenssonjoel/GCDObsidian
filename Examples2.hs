
module Examples2 where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.C as C
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

test1 :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
test1 = pure (block 512) ->- pure rev ->- sync ->-
        pure (unblock . push) 

getTest1 = putStrLn$ CUDA.genKernelGlob "test1" test1 (GlobalArray undefined :: GlobalArray Pull (Exp Int)) 


testParam1 :: (GlobalArray Pull (Exp Int), Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
testParam1 (garr, param) = res
  where 
    res = test1$ fmap (+param) garr 

getTestParam1 = putStrLn$ CUDA.genKernelGlob "testParam1" testParam1 (GlobalArray undefined :: GlobalArray Pull (Exp Int),variable "v") 

{- 
   A kernel Takes a global Pull array as input 
   and Pushes a global array as output. 
   
   There should be no way to go from a Global Push array 
   to a global Pull inside a kernel.  

   TODO: All Kernel generation functions need to 
         be updated! 
   TODO: Implement vSwap iSwap as example usages 
         of our Global Arrays. 
   TODO: Parameters to kernels that are not Arrays! 
         (should not be too tricky to add) 

   (Is a third kind of array needed ? a Mutable 
    storage location kind of array ?) 
-} 