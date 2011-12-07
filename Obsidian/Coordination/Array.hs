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

    - What information are useful in the coordination layer. 
         # number of elements a Kernel operates on is more useful than number of threads. 
             - But a kernel might take many input arrays 
               and those may be of different length. (for example a kernel taking a 256 elements 
               array and an 128 element array)

   
   -- TODO: Something like this ? (Ok ?) 
      runKC :: (GlobalArray a -> KC (GlobalArray b)) -> SomeKindOfHaskellArray a -> SomeKindOfHaskellArray 






   -- TODO: Look at all of this again, with the new "GlobalArrays" 
      from Obsidian.GCDObsidian.Arrays in mind. 
      
   -- TODO: ALSO! see if the new version of Foreign.CUDA 
        works properly on never CUDA versions. If it does 
        se if it can be used to launch Obsidian kernels somehow. 
-} 



module Obsidian.Coordination.Array where 

import Obsidian.Coordination.CoordC -- future im representation


import Obsidian.GCDObsidian.Exp

import Obsidian.GCDObsidian.Kernel
import Obsidian.GCDObsidian.Array
import qualified Obsidian.GCDObsidian.Library as Lib
import Obsidian.GCDObsidian.Sync 
import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Library


import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA 
import qualified Obsidian.GCDObsidian.CodeGen.InOut as InOut
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

import Control.Monad.Writer

import Data.Word
import qualified Data.Map as Map

import Prelude hiding (zipWith)


import Control.Monad.State
import Control.Monad.Writer

bid :: Exp Word32
bid = variable "bid"
bwd :: Exp Word32
bwd = variable "blockDim.x"
gwd :: Exp Word32
gwd = variable "gridDim.x" 

standardInput :: Array (Exp Int)
standardInput = Array (\tix-> index "input" ((bid*bwd)+tix)) 256

revblocks :: Array a -> Array a 
revblocks (Array ixf n) = Array (\tix -> ixf (((gwd - bid - 1)*(fromIntegral n)) + tix)) n

stdIn :: Word32 -> Exp Word32 -> Exp Word32 
stdIn n tix  = (bid*(fromIntegral n)) + tix 

stdOut :: Word32 -> Exp Word32 -> Exp Word32
stdOut n tix = (bid *(fromIntegral n)) + tix

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
              -> (Exp Word32 -> Exp Word32)  -- Transform array on input
              -> (Array (Exp a) -> Kernel (Array (Exp b))) -- kernel to apply
              -> (Exp Word32 -> Exp Word32) -- Transform array on output 
              -> KC (GlobalArray (Exp a))  -- Input array 
              -> KC (GlobalArray (Exp b))  -- Result array 
                
  WriteResult :: Int -> Int -> KC (GlobalArray a) -> KC () 
{- TODO: This seems very hard to expand upon. 
         Just adding a "LaunchBin" turns complicated. 
  
         Some other way of taking care of the in and out transformations are needed. 
         Would be cool if the transformation could be "attached" to the GlobalArrays
         in some way. The problem is that the code that implements the transformation 
         must be internalized into the kernel (That is why they are now specified 
         like this).          
-} 


----------------------------------------------------------------------------             

-- Kernel code to Kernel code map ... awful right ?  
type KernelMap = Map.Map String (String, String, Word32,  Word32) 
--                               code    name    threads  shared
----------------------------------------------------------------------------     
-- generate coordination code + kernels 


run :: (GlobalArray (Exp Int) -> KC ()) -> (KernelMap,String)
run coord = (km,head ++ body ++ end )
  where 
    ((_,_,km),body) = runKCM (coord (GlobalArray undefined)) (0,0) (Map.empty)
    head = "void coord(int *input0, int input0size,int *output0, int output0size){\n"
    end  = "\n}"


run_ k = 
    "/* Kernels */\n" ++
    concat  kernels ++
    "/* Coordination */\n" ++
    prg 
      where 
        kernels = map (\(x,_,_,_) -> x) (Map.elems km) -- 
        (km,prg) = run k 
        

run' k = 
  do 
    putStrLn "/* Kernels */"  
    sequence_$ map putStrLn kernels
    putStrLn "/* Coordination */" 
    putStrLn prg     
      where 
        kernels = map (\(x,_,_,_) -> x) (Map.elems km) -- 
        ((_,_,km),prg) = runKCM k (0,0) (Map.empty)                 
        
        
-- will only work for integers right now...  (fix , how )                 
runKCM :: KC a -> (Int,Int) -> KernelMap -> ((a,(Int,Int),KernelMap) ,String)
runKCM (Input arr) (ai,ki) km = ((GlobalArray ai,(ai+1,ki),km) ,allocate ++ copy)
   where 
     allocate = allocInput (GlobalArray ai)
     copy     = copyInput  (GlobalArray ai)
       

runKCM (WriteResult blocks elems arr) ids km = (((),ids',km'),prg ++ (writeResult res blocks elems)) -- CHEATING
  where ((res,ids',km'),prg) = runKCM arr ids km 
runKCM (LaunchUn blocks elems inf k outf i) ids km = result
  where 
    
    -- Generate kernel for lookup purposese
    kern = ((pure inputTransform ->- k ->- pOutput outf) (Array (\ix -> index "input0" ix) (fromIntegral elems)))
    inputTransform = \arr -> Array (\ix -> arr ! (inf ix)) (len arr)
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
              kernelIns' =  CUDA.genKernel_ ("gen"  ++ show id)
                             kern
                             [("input0",Pointer Int)]  -- type ??
                             [("output0",Pointer Int)] 
              (str,threads,sm) = kernelIns'                     
              kernelIns = (str,"gen" ++ show id,threads, sm) -- TODO CLEANUP 
                              
          in  ((fst ids',id+1),Map.insert kernel kernelIns km', call ("gen" ++ show id) blocks threads sm (idOf res) (idOf newImm) ) --add one kernel 
        (Just (_,kernNom,threads,sm)) -> (ids',km',call kernNom blocks threads sm (idOf res) (idOf newImm)) --return excisting
     
 
     -- Generate the input to this stage  
    ((res,ids', km'),prg) = runKCM i ids km
    newImm = GlobalArray (fst newids)
    allocprg = allocImm newImm (fromIntegral (len outArr)*blocks) 
    
    result = ((newImm ,(fst newids +1,snd newids),newkm),allocprg ++ "\n"++ prg ++ " \n" ++ newprg ++ "\n")
  
allocInput (GlobalArray id) = 
  "  int* dinput"++ show id ++ ";\n" ++ 
  "  cudaMalloc((void**)&dinput" ++ show id ++ ", sizeof(int) * input" ++ show id ++ "size );\n"
         
copyInput (GlobalArray id) = 
  "  cudaMemcpy(dinput"++ sid ++ ",input" ++ sid ++", sizeof(int) * input"++sid++"size, cudaMemcpyHostToDevice);\n" 
  where sid = show id
  
allocImm (GlobalArray id) s= 
  "  int* dinput"++ show id ++ ";\n" ++ 
  "  cudaMalloc((void**)&dinput" ++ show id ++ ", sizeof(int) * "++ show s ++ ");\n"

  
  
-- Shared memory usage also needed
call name blocks threads sm input output = 
   "  " ++ name ++ "<<<"++ show blocks ++", " ++ show threads ++" ," ++show sm++ " >>>((int*)dinput" ++ show input ++ ",(int*)dinput" ++ show output ++ ");\n" 

writeResult (GlobalArray id) blocks elems = 
   "  cudaMemcpy(output0, dinput"++show id++", sizeof(int) * "++ show (elems * blocks) ++" , cudaMemcpyDeviceToHost);\n"

 
----------------------------------------------------------------------------
-- 
pOutput  :: Scalar a => (Exp Word32 -> Exp Word32) -> Array (Exp a) -> Kernel (Array (Exp a))
pOutput outf arr = 
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
    globalTarget n blockSize (i,a) = Assign n (outf i)   a 
  
  
----------------------------------------------------------------------------
-- tests.
test :: GlobalArray (Exp Int) -> KC ()
test arr = let arr' = Input arr 
               imm  = LaunchUn 10 256 (stdIn 256) myKern (stdOut 256) arr'
               imm2 = LaunchUn 10 256 (stdIn 256) myKern (stdOut 256) imm
            in WriteResult 10 256 imm2
                
               
{-
launchUn :: (Scalar a, Scalar b) 
            => Int                  
            -> Int                  
            -> (Exp Word32 -> Exp Word32)  -- Maybe should be something else ? (bid tid etc ?) 
            -> (Array (Exp a) -> Kernel (Array (Exp b))) 
            -> (Exp Word32 -> Exp Word32)  
            -> KC (GlobalArray (Exp a)) 
            -> KC (GlobalArray (Exp b)) 
launchUn blocks threads inf kern outf input =                
  LaunchUn blocks threads id (pure inf ->- kern ->- pure outf) id input

test2 :: GlobalArray (Exp Int) -> KC ()
test2 arr = let arr' = Input arr 
                imm  = launchUn 10 256 stdIn myKern id arr'
                imm2 = launchUn 10 256 revblocks myKern id imm
             in WriteResult 10 256 imm2
-}                        
               
               
               
               
----------------------------------------------------------------------------               
-- ReductionTest


-- general reductions
reduce :: Syncable Array a => (a -> a -> a) -> Array a -> Kernel (Array a)
reduce op arr | len arr == 1 = return arr
              | otherwise    = 
                (pure ((uncurry (zipWith op)) . halve)
                 ->- sync
                 ->- reduce op) arr
               
-- reduction kernel for integers.
reduceAddInt :: Array (Exp Int) -> Kernel (Array (Exp Int))                 
reduceAddInt = reduce (+) 

-- Coordination code.
reduceLarge :: GlobalArray (Exp Int) -> KC () 
reduceLarge arr = let arr' = Input arr 
                      imm  = LaunchUn 256 256 (stdIn 256) reduceAddInt (stdOut 1) arr' 
                      imm2 = LaunchUn 1   256 (stdIn 256) reduceAddInt (stdOut 1) imm 
                  in  WriteResult 1 1 imm2
                      
-- output cuda as a string 
getReduceLarge = putStrLn$ run_ reduceLarge




----------------------------------------------------------------------------
-- Experiment2 



    
data CPUArray a = CPUArray {cpuArrayName :: String,
                            cpuArrayType :: Type,
                            cpuArraySize :: Exp Word32}
                  
data GPUArray a = GPUArray {gpuArrayName :: String,
                            gpuArrayType :: Type,
                            gpuArraySize :: Exp Word32,
                            gpuArrayPerm :: (Exp Word32 -> Exp Word32)}
    
data M a = M a                 
instance Monad M where 
  return a = M a 
  (>>=) (M a) f = f a 
  
  

  
data CoordState = CoordState {kernelMap :: KernelMap,   
                              nextId    :: Integer}
--Why do I always fall into doing things this way ? 
type Coord a = StateT CoordState (Writer CoordC)  a

newId :: Coord Integer
newId = do 
  s <- get 
  let i = nextId s 
  put (s {nextId = i+1})
  return i
  
--    (String, String, Word32,  Word32) 
--     code    name    threads  shared       
  
  
--               code       code   name                           name 
insertKernel :: String -> (String,String,Word32,Word32) -> Coord ()
insertKernel prot cand = 
  do 
    s <- get 
    let km = kernelMap s 
        km' = Map.insert prot cand km 
    put (s {kernelMap = km'})
    
lookupKernel :: String -> Coord (Maybe (String,String,Word32,Word32)) 
lookupKernel prot = 
  do 
    s <- get 
    let km = kernelMap s 
    return (Map.lookup prot km) 
    
          
                  
copyIn :: GPUArray e -> CPUArray e -> Coord () 
copyIn gpuArr cpuArr = 
  do    
    let gpuV = (CVar (gpuArrayName gpuArr) (gpuArrayType gpuArr))
        cpuV = (CVar (cpuArrayName cpuArr) (cpuArrayType cpuArr)) 
        code = MemCpy gpuV cpuV (gpuArraySize gpuArr)
    tell code               
  
        

copyOut :: CPUArray e -> GPUArray e -> Coord ()
copyOut cpuArr gpuArr = 
  do    
    let gpuV = (CVar (gpuArrayName gpuArr) (gpuArrayType gpuArr))
        cpuV = (CVar (cpuArrayName cpuArr) (cpuArrayType cpuArr)) 
        code = MemCpy cpuV gpuV (cpuArraySize cpuArr)
    tell code               
  

newArr :: Type -> Exp Word32 -> Coord (GPUArray e) 
newArr t bytes = 
  do 
    ident <- newId 
    let gpuV = (CVar ("imm"++show ident) t)
    tell $ Malloc gpuV bytes
    return (GPUArray ("imm"++show ident)
                     t
                     bytes 
                     id)

launch :: Word32 
          -> Word32 
          -> (Array (Exp a) -> Kernel (Array (Exp b)))
          -> GPUArray a 
          -> GPUArray b
          -> Coord ()
launch = undefined 
          
         
permute :: (Exp Word32 -> Exp Word32) -> GPUArray e -> Coord (GPUArray e) 
permute = undefined 
         
silly1 arr out =          
  do 
    arr' <- newArr (cpuArrayType arr) (10*256*4)
    
    copyIn arr' arr 
    
    imm  <- newArr Int (10*256*4)  -- Array with type of return type of kernel
    imm2 <- newArr Int (10*256*4)  
    
    launch 10 256 (sync) arr' imm 
    
    imm' <- permute (\ix -> 10*256 - 1 - ix) imm 
    launch 10 256 (sync) imm' imm2 
    
    copyOut out imm2
    
    

         
reduceLarge2 arr out =  
  do
    arr' <- newArr (cpuArrayType arr) (256*256*4)
    
    copyIn  arr' arr 
    imm  <- newArr Int (256*4) 
    imm2 <- newArr Int (1*4)    
    
    launch 256 256 reduceAddInt arr' imm 
    
    launch 1   256 reduceAddInt imm  imm2 
    
    copyOut out imm2 
         