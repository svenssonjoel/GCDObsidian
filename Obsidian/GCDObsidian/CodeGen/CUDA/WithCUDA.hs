

module Obsidian.GCDObsidian.CodeGen.CUDA.WithCUDA where

import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Foreign.CUDA.Analysis.Device as CUDA


import Obsidian.GCDObsidian.CodeGen.CUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.Compile
import Obsidian.GCDObsidian.CodeGen.InOut

import Control.Monad.State

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?) 

---------------------------------------------------------------------------
-- Get a list of devices from the CUDA driver
---------------------------------------------------------------------------
getDevices :: IO [(CUDA.Device,CUDA.DeviceProperties)]
getDevices = do
  num <- CUDA.count 
  devs <- mapM CUDA.device [0..num-1]
  props <- mapM CUDA.props devs
  return $ zip devs props

---------------------------------------------------------------------------
-- Print a Summary of a device's properties. 
---------------------------------------------------------------------------
propsSummary :: CUDA.DeviceProperties -> String
propsSummary props = unlines
  ["Device Name: " ++ CUDA.deviceName props,
   "Compute Cap: " ++ show (CUDA.computeCapability props),
   "Global Mem:  " ++ show (CUDA.totalGlobalMem props),
   "Shared Mem/Block: " ++ show (CUDA.sharedMemPerBlock props),
   "Registers/Block: "  ++ show (CUDA.regsPerBlock props),
   "Warp Size: " ++ show (CUDA.warpSize props),
   "Max threads/Block: " ++ show (CUDA.maxThreadsPerBlock props),
   "Max threads/MP: " ++ show (CUDA.maxThreadsPerMultiProcessor props),
   "Clock rate: " ++ show (CUDA.clockRate props),
   "Num MP: " ++ show (CUDA.multiProcessorCount props),
   "Mem bus width: " ++ show (CUDA.memBusWidth props)] 

---------------------------------------------------------------------------
-- Environment to run CUDA computations in.
--  # Needs to keep track of generated and loaded functions etc. 
---------------------------------------------------------------------------

data CUDAState = CUDAState { csIdent :: Int,
                             csCtx   :: CUDA.Context }

type CUDA a =  StateT CUDAState IO a

newIdent :: CUDA Int
newIdent =
  do
    i <- return . csIdent =<< get
    modify (\s -> s {csIdent = i+1 }) 
    return i
  
withCUDA p =
  do
    CUDA.initialise []
    devs <- getDevices
    case devs of
      [] -> error "No CUDA device found!" 
      (x:xs) ->
        do 
          ctx <- CUDA.create (fst x) [CUDA.SchedAuto] 
          runStateT p (CUDAState 0 ctx) 
          CUDA.destroy ctx


---------------------------------------------------------------------------
-- Capture and compile a Obsidian function into a CUDA Function
---------------------------------------------------------------------------
capture :: ToProgram a b => (a -> b) -> Ips a b -> CUDA CUDA.Fun 
capture f inputs =
  do
    i <- newIdent
    let kn     = "gen" ++ show i
        fn     = kn ++ ".cu"
        cub    = fn ++ ".cubin" 
        prgstr = genKernel kn f inputs 
        header = "#include <stdint.h>\n" -- more includes ? 
         
    lift $ storeAndCompile (fn) (header ++ prgstr)

    mod <- liftIO $ CUDA.loadFile cub
    fun <- liftIO $ CUDA.getFun mod kn 

    {- After loading the binary into the running process
       can I delete the .cu and the .cu.cubin ? -} 
           
    return fun


---------------------------------------------------------------------------
-- useVector: Copies a Data.Vector from "Haskell" onto the GPU Global mem 
--------------------------------------------------------------------------- 
useVector :: V.Storable a =>
             V.Vector a -> (CUDA.DevicePtr a -> CUDA b) -> CUDA b
useVector v f =
  do
    let (hfptr,n) = V.unsafeToForeignPtr0 v
    
    dptr <- lift $ CUDA.mallocArray n
    let hptr = unsafeForeignPtrToPtr hfptr
    lift $ CUDA.pokeArray n hptr dptr
    b <- f dptr     
    lift $ CUDA.free dptr
    return b

---------------------------------------------------------------------------
-- allocaVector: allocates room for a vector in the GPU Global mem
---------------------------------------------------------------------------
allocaVector :: V.Storable a => 
                Int -> (CUDA.DevicePtr a -> CUDA b) -> CUDA b
allocaVector n f =
  do
    dptr <- lift $ CUDA.mallocArray n
    b <- f dptr
    lift $ CUDA.free dptr
    return b 


