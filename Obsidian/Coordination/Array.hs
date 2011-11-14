{-# Language GADTs, 
             FlexibleContexts #-}

{-  
  TODO: 
    - Correctly get number of threads, lengths (Cheating with this all over).
    - Support kernels that takes other arrays than just Arrays of int.
    
    - Output to the intermediate CoordC language instead of to strings.

    - GlobalArrays as they are now, just an identifier, or something like
        GlobalArray Name Size Type Etc
    
    - Give functions proper function-head
    - How to call the resulting coordinations function from Haskell

    - input is not fetched from the correct place if id is used as input transform

    - Figure out what parameters the coordination function should take 
        for good flexibility (The generated C function that calls kernels)

-} 



module Obsidian.Coordination.Array where 

import Obsidian.Coordination.CoordC -- future im representation


import Obsidian.GCDObsidian.Exp

import Obsidian.GCDObsidian.Kernel
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

standardInput :: Array (Exp Int)
standardInput = Array (\tix-> index "input" ((bid*bwd)+tix)) 256

revblocks :: Array a -> Array a 
revblocks (Array ixf x) = Array (\tix -> ixf ((gwd - bid - 1) + tix)) x


----------------------------------------------------------------------------
-- 

myKern :: Array (Exp Int) -> Kernel (Array (Exp Int))
myKern = pure Lib.rev ->- sync  ->- sync ->- sync                                           
  


 
----------------------------------------------------------------------------
-- 
newtype GlobalArray a = GlobalArray Int -- Just an identifier
-- data GlobalArray a = GlobalArray Name Size Type Etc 
idOf (GlobalArray i) =  i 


data KC a where 
  Input :: GlobalArray a -> KC (GlobalArray a)
  
  -- Map a single input array - single output array kernel over a global array  
  LaunchUn :: (Scalar a, Scalar b) 
              => Int                   -- number of blocks  (Could be a runtime value, perhaps)
              -> Int                   -- number of Elements that this kernel process
              -> (Array (Exp a) -> Array (Exp a))  -- Transform array on input
              -> (Array (Exp a) -> Kernel (Array (Exp b))) -- kernel to apply
              -> (Array (Exp b) -> Array (Exp b))  -- Transform array on output 
              -> KC (GlobalArray (Exp a))  -- Input array 
              -> KC (GlobalArray (Exp b))  -- Result array 
  WriteResult :: KC (GlobalArray a) -> KC () 
             


----------------------------------------------------------------------------             

-- Kernel code to Kernel code map ... awful right ?  
type KernelMap = Map.Map String (String, Word32,  Word32) 
--                               code    threads  shared
----------------------------------------------------------------------------     
-- generate coordination code + kernels 


run :: (GlobalArray (Exp Int) -> KC ()) -> (KernelMap,String)
run coord = (km,head ++ body ++ end )
  where 
    ((_,_,km),body) = runKCM (coord (GlobalArray undefined)) (0,0) (Map.empty)
    head = "void coord(int *input0, int input0size,int *output0, int output0size);\n{"
    end  = "\n}"


run_ k = 
    "/* Kernels */\n" ++
    concat  kernels ++
    "/* Coordination */\n" ++
    prg 
      where 
        kernels = map (\(x,_,_) -> x) (Map.elems km) -- 
        (km,prg) = run k 
        

run' k = 
  do 
    putStrLn "/* Kernels */"  
    sequence_$ map putStrLn kernels
    putStrLn "/* Coordination */" 
    putStrLn prg     
      where 
        kernels = map (\(x,_,_) -> x) (Map.elems km) -- 
        ((_,_,km),prg) = runKCM k (0,0) (Map.empty)                 
        
        
-- will only work for integers right now...  (fix , how )                 
runKCM :: KC a -> (Int,Int) -> KernelMap -> ((a,(Int,Int),KernelMap) ,String)
runKCM (Input arr) (ai,ki) km = ((GlobalArray ai,(ai+1,ki),km) , allocInput (GlobalArray ai)) 
runKCM (WriteResult arr) ids km = (((),ids',km'),prg ++ (writeResult res 256)) -- CHEATING
  where ((res,ids',km'),prg) = runKCM arr ids km 
runKCM (LaunchUn blocks elems inf k outf i) ids km = result
  where 
    
    -- Generate kernel for lookup purposese
    kern = ((pure inf ->- k ->- pure outf ->- pOutput) (Array (\ix -> index "input0" ix) (fromIntegral elems)))
    ((outArr,_),_) = runKernel kern
    (kernel,_,_) = CUDA.genKernel_ "gen" 
                             kern
                             [("input0",Int)]  -- type ??
                             [("output0",Int)] 
    
                             
    (newids,newkm,newprg) = 
      -- Has Kernel already been generated ? 
      case Map.lookup kernel km' of 
        Nothing -> 
          let id = snd ids'  
              -- Generate kernel again. with different name, for insertion. 
              -- (This should be improved upon) 
              kernelIns =  CUDA.genKernel_ ("gen"  ++ show id)
                             kern
                             [("input0",Pointer Int)]  -- type ??
                             [("output0",Pointer Int)] 
              (_,threads,sm) = kernelIns               
                              
          in  ((fst ids',id+1),Map.insert kernel kernelIns km', call ("gen" ++ show id) blocks threads sm (idOf res) (idOf newImm) ) --add one kernel 
        (Just (kernNom,threads,sm)) -> (ids',km',call kernNom blocks threads sm (idOf res) (idOf newImm)) --return excisting
     
 
     -- Generate the input to this stage  
    ((res,ids', km'),prg) = runKCM i ids km
    newImm = GlobalArray (fst newids)
    allocprg = allocImm newImm (fromIntegral (len outArr)*blocks) 
    
    result = ((newImm ,(fst newids +1,snd newids),newkm),allocprg ++ "\n"++ prg ++ " \n" ++ newprg ++ "\n")
  
allocInput (GlobalArray id) = 
  "  int* dinput"++ show id ++ ";\n" ++ 
  "  cudaMalloc((void**)&dinput" ++ show id ++ ", sizeof(int) * input" ++ show id ++ "size );\n"
         
allocImm (GlobalArray id) s= 
  "  int* dinput"++ show id ++ ";\n" ++ 
  "  cudaMalloc((void**)&dinput" ++ show id ++ ", sizeof(int) * "++ show s ++ ");\n"

  
  
-- Shared memory usage also needed
call name blocks threads sm input output = 
   "  " ++ name ++ "<<<"++ show blocks ++", " ++ show threads ++" ," ++show sm++ " >>>((int*)dinput" ++ show input ++ ",(int*)doutput" ++ show output ++ ");\n" 

writeResult (GlobalArray id) size = 
   "  cudaMemcpy(output0, dinput"++show id++", sizeof(int) * "++ show size ++" , cudaMemcpyDeviceToHost);\n"

    
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
test :: GlobalArray (Exp Int) -> KC ()
test arr = let arr' = Input arr 
               imm  = LaunchUn 10 256 id myKern id arr'
               imm2 = LaunchUn 10 256 revblocks myKern id imm
            in WriteResult imm2
                