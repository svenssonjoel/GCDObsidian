{-# LANGUAGE TypeOperators #-} 
module Obsidian.GCDObsidian.CodeGen.CUDA.WithCUDA where

import qualified Foreign.CUDA.Driver as CUDA
import qualified Foreign.CUDA.Driver.Device as CUDA
import qualified Foreign.CUDA.Analysis.Device as CUDA
import qualified Foreign.CUDA.Driver.Stream as CUDAStream 

import Obsidian.GCDObsidian.CodeGen.CUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.Compile
import Obsidian.GCDObsidian.CodeGen.InOut

import Control.Monad.State

import qualified Data.Vector.Storable as V
import Foreign.Marshal.Array
import Foreign.ForeignPtr.Unsafe -- (req GHC 7.6 ?) 


import Data.Word
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
                             csCtx   :: CUDA.Context,
                             csProps :: CUDA.DeviceProperties}

type CUDA a =  StateT CUDAState IO a

data Kernel = Kernel {kFun :: CUDA.Fun,
                      kThreadsPerBlock :: Word32 } 

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
          runStateT p (CUDAState 0 ctx (snd x)) 
          CUDA.destroy ctx


---------------------------------------------------------------------------
-- Capture and compile a Obsidian function into a CUDA Function
---------------------------------------------------------------------------
capture :: ToProgram a b => (a -> b) -> Ips a b -> CUDA Kernel 
capture f inputs =
  do
    i <- newIdent

    props <- return . csProps =<< get
    
    let kn     = "gen" ++ show i
        fn     = kn ++ ".cu"
        cub    = fn ++ ".cubin"
        prgThreads = getNThreads f inputs
        prgstr = genKernel kn f inputs 
        header = "#include <stdint.h>\n" -- more includes ? 
         
    lift $ storeAndCompile (archStr props) (fn) (header ++ prgstr)

    mod <- liftIO $ CUDA.loadFile cub
    fun <- liftIO $ CUDA.getFun mod kn 

    {- After loading the binary into the running process
       can I delete the .cu and the .cu.cubin ? -} 
           
    return $ Kernel fun prgThreads

archStr :: CUDA.DeviceProperties -> String
archStr props = "-arch=sm_" ++ archStr' (CUDA.computeCapability props)
  where
    archStr' (1.0) = "10"
    archStr' (1.2) = "12"
    archStr' (2.0) = "20" 
    archStr' (3.0) = "30"
    -- archStr' x = error $ show x 
    

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


---------------------------------------------------------------------------
-- execute Kernels on the GPU 
---------------------------------------------------------------------------
execute :: (ParamList a, ParamList b) => Kernel
           -> Word32 -- Number of blocks 
           -> Word32 -- Amount of Shared mem (get from an analysis) 
         --  -> Maybe CUDAStream.Stream
           -> a -> b
           -> CUDA ()
execute k nb sm {- stream -} a b = lift $ 
  CUDA.launchKernel (kFun k)
                    (fromIntegral nb,1,1)
                    (fromIntegral (kThreadsPerBlock k), 1, 1)
                    (fromIntegral sm)
                    Nothing -- stream
                    (toParamList a ++ toParamList b) -- params

---------------------------------------------------------------------------
-- ParamList
---------------------------------------------------------------------------

class ParamList a where
  toParamList :: a -> [CUDA.FunParam]

instance ParamList (CUDA.DevicePtr a) where
  toParamList a = [CUDA.VArg a]


instance (ParamList a, ParamList b) => ParamList (a :-> b) where
  toParamList (a :-> b) = toParamList a ++ toParamList b 
