
module Examples2 where 

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import qualified Obsidian.GCDObsidian.CodeGen.C as C
import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import Obsidian.GCDObsidian.Program

import Data.Word
import Data.Bits

import Prelude hiding (zipWith)

----------------------------------------------------------------------------
--

test1 :: GlobalArray Pull (Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
test1 = pure (block 256) ->- pure (fst . halve) ->- 
        pure (unblock . push) 

getTest1 = putStrLn$ CUDA.genKernelGlob "test1" test1 (GlobalArray undefined (variable "n"):: GlobalArray Pull (Exp Int)) 


testParam1 :: (GlobalArray Pull (Exp Int), Exp Int) -> Kernel (GlobalArray Push (Exp Int)) 
testParam1 (garr, param) = res
  where 
    res = test1$ fmap (+param) garr 

getTestParam1 = putStrLn$ CUDA.genKernelGlob "testParam1" testParam1 (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "v") 


globRev (GlobalArray (Pull ixf) n) = GlobalArray (Pull (\ix -> ixf (n - 1 - ix))) n

testGlobRev = pure globRev ->- pure (block 256) ->- pure (unblock . push) 

getTestGlobRev = putStrLn$ CUDA.genKernelGlob "testGlobRev" testGlobRev (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int)) 


vSwap :: (GlobalArray Pull (Exp Int), Exp Word32) -> 
         Kernel (GlobalArray Push (Exp Int)) 
vSwap (arr,stride) = return p5
    
  where 
    t1 ix = ix + (ix .&. (complement (stride - 1)))
    t2 ix = ix `xor` ((stride `shiftL` 1)-1)
    arr1  = GlobalArray (Pull (\ix -> arr ! t1 ix)) (globLen arr `div` 2)
    arr2  = GlobalArray (Pull (\ix -> arr ! t2 ix)) (globLen arr `div` 2)
    arr1' = zipWithG min arr1 arr2
    arr2' = zipWithG max arr1 arr2
    p1    = pushGlobal 512 arr1'
    p2    = pushGlobal 512 arr2'
    p3    = ixMapGlobal t1 p1 
    p4    = ixMapGlobal t2 p2 
    p5    = GlobalArray (Push (\k -> p3 !** k *>* p4 !** k)) (globLen arr)
    
infixl 9 !** 
(!**) (GlobalArray (Push f) _) a = f a                                      
                                     
zipWithG op a1 a2 =  
  GlobalArray (Pull  (\ix -> (a1 ! ix) `op` (a2 ! ix)))
                   (min (globLen a1) (globLen a2))

pushGlobal blocksize = 
   unblock . push . block blocksize   
  
    
getvSwap = putStrLn$ CUDA.genKernelGlob "vSwap" vSwap (GlobalArray undefined (variable "n") :: GlobalArray Pull (Exp Int),variable "stride")     
    
--pushGlobal (GlobalArray (Pull ixf) n) = 
--   GlobalArray (Push (\func -> ForAllGlobal (\i -> func (i,(ixf i))) n)) n
      
{- 
__global__ void vSwap(
    int *d_input,
    int *d_output
    unsigned int stride){

    unsigned int tid = blockIdx.x * blockDim.x + threadIdx.x;
   
    unsigned int ix = tid + (tid & ~(stride - 1));

    unsigned int ix2 = ix^((stride<<1)-1);
    
    int v1 = d_input[ix];
    int v2 = d_input[ix2];

    d_output[ix] = min(v1,v2);
    d_output[ix2] = max(v1,v2);
    
} -} 

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