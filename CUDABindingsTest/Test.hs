{-# LANGUAGE ScopedTypeVariables #-} 


module Test where

--port Foreign.CUDA 

import Foreign.CUDA.Driver as CUDA
import Data.Ord
import Data.List
import Data.Function
import Foreign.CUDA.Driver.Device
import Foreign.CUDA.Analysis.Device
import Foreign.Storable
import qualified Foreign.Storable.Traversable as Store


import Foreign.Marshal.Array

import Data.Int

instance Storable a => Storable [a] where
  sizeOf = Store.sizeOf
  alignment = Store.alignment
  peek = Store.peek (error "instance Traversable [a] is lazy, so we do not provide a real value here")
  poke = Store.poke

  
selectFirstDevice :: IO (Device, DeviceProperties)
selectFirstDevice = do
  dev <- mapM CUDA.device . enumFromTo 0 . subtract 1 =<< CUDA.count
  prop <- mapM CUDA.props dev
  return . head $ zip dev prop


main =
  do
    CUDA.initialise []
    (dev,prp) <- selectFirstDevice
    ctx       <- CUDA.create dev [CUDA.SchedAuto] 

    putStrLn $ deviceName prp
    putStrLn $ show $ computeCapability prp 
                 
    mymodule <- loadFile "test.cubin"
    fun <- getFun mymodule "two"

    
    -- remember that the "int" type on the device  is 32bits. 
    r <- CUDA.allocaArray 32 $ \(inp :: DevicePtr Int32) ->
          CUDA.allocaArray 32 $ \(out :: DevicePtr Int32) ->
          do
            CUDA.pokeListArray [0..31::Int32] inp 
            launchKernel fun (1,1,1) (32,1,1) (32*8) Nothing [VArg inp, VArg out]
            peekListArray 32 out
            
    putStrLn $ show r 
    return ()




