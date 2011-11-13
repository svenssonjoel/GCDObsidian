{-# Language GADTs, 
             FlexibleContexts #-}

module Obsidian.Coordination.Array where 


import Obsidian.GCDObsidian.Exp

import Obsidian.GCDObsidian.Kernel
import qualified Obsidian.GCDObsidian.Array as Shared
import Obsidian.GCDObsidian.Array
import qualified Obsidian.GCDObsidian.Library as Lib
import Obsidian.GCDObsidian.Sync 
import Obsidian.GCDObsidian.Program


import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA 
import qualified Obsidian.GCDObsidian.CodeGen.InOut as InOut
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

import Control.Monad.Writer

import Data.Word
import qualified Data.Map as Map



bid :: Exp Word32
bid = variable "blockIdx.x"
bwd :: Exp Word32
bwd = variable "blockDim.x"
gwd :: Exp Word32
gwd = variable "gridDim.x" 

standardInput :: Array (Exp Word32)
standardInput = Array (\tix-> index "input" ((bid*bwd)+tix)) 256

revblocks :: Array a -> Array a 
revblocks (Array ixf x) = Array (\tix -> ixf ((gwd - bid - 1) + tix)) x


----------------------------------------------------------------------------
-- 

myKern :: Shared.Array (Exp Word32) -> Kernel (Shared.Array (Exp Word32))
myKern = pure Lib.rev ->- sync  ->- sync ->- sync                                           
  


 
----------------------------------------------------------------------------
-- 
newtype GlobalArray a = GlobalArray Int -- Just an identifier
-- data GlobalArray a = GlobalArray Name Size Etc


data KC a where 
  Input :: GlobalArray a -> KC (GlobalArray a) 
  
  -- Map a single input array - single output array kernel over a global array  
  LaunchUn :: (Scalar a, Scalar b) 
              => Int                   -- number of blocks 
              -> Int                   -- number of Elements that this kernel process
              -> (Array (Exp a) -> Array (Exp a))  -- Transform array on input
              -> (Array (Exp a) -> Kernel (Array (Exp b))) -- kernel to apply
              -> (Array (Exp b) -> Array (Exp b))  -- Transform array on output 
              -> KC (GlobalArray (Exp a))  -- Input array 
              -> KC (GlobalArray (Exp b))  -- Result array 
             


             
----------------------------------------------------------------------------
-- Run a KC 
run k = putStrLn$ snd$ runKC k  

runKC :: KC a -> (a,String) 
runKC (Input arr) = (arr,"An input\n")

runKC (LaunchUn blocks elems inf k outf i) = (GlobalArray 100,prev ++ launch ++  kernel) 
   where                       
     launch = "Launching a Kernel: \n"
     (inp,prev) = runKC i
     kernel = CUDA.genKernel_ "generated" 
                              ((pure inf ->- k ->- pure outf ->- pOutput) (Array (\ix -> index "input0" ix) (fromIntegral elems)))
                              (fromIntegral elems) -- assumes length == threads  
                              [("input0",Word32)]
                              [("output0",Word32)] 
     
     
type KernelMap = Map.Map String String -- Kernel code to function name, Awful right ? 
                                       -- But there is no Eq on (a -> Kernel b) 
     
runKCM :: (GlobalArray a -> KC (GlobalArray b) -> KernelMap -> ((GlobalArray b) ,String)
runKCM = undefined 


-- TODO: Something like this ? (Ok ?) 
{- 
runKC :: (GlobalArray a -> KC (GlobalArray b)) -> SomeKindOfHaskellArray a -> SomeKindOfHaskellArray b

    
-} 
----------------------------------------------------------------------------
-- 
pOutput  :: Scalar a => Array (Exp a) -> Kernel (Array (Exp a))
pOutput arr = 
  do 
    let name = "output0"
    let p = pushApp parr (globalTarget name (fromIntegral (len arr)))
        
    tell p 
            
    return (Array (index name) (len arr))
      
  where 
    es = fromIntegral$ sizeOf (arr ! 0) 
    t  = Pointer$ Local$ typeOf (arr ! 0)

    parr = push arr 
    globalTarget :: Scalar a => Name -> Exp Word32 -> (Exp Word32, Exp a) -> Program ()
    globalTarget n blockSize (i,a) = Assign n ((bid * blockSize) + i)  a 
  
  
----------------------------------------------------------------------------
-- tests.
    
test2 :: GlobalArray (Exp Word32) -> KC (GlobalArray (Exp (Word32))) 
test2 arr = let arr' = Input arr 
                imm  = (LaunchUn  10 256 id myKern id arr')
            in  (LaunchUn 10 256 revblocks myKern id imm) 
                
                