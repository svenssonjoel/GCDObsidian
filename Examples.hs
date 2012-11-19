{-# LANGUAGE FlexibleInstances, 
             ScopedTypeVariables,
             RankNTypes  #-} 
module Examples where 

--import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.WithCUDA
import Obsidian.GCDObsidian.CodeGen.CUDA.Compile
import qualified Foreign.CUDA.Driver as CUDA
--import qualified Obsidian.GCDObsidian.CodeGen.C as C
--import qualified Obsidian.GCDObsidian.CodeGen.OpenCL as CL

import qualified Obsidian.GCDObsidian.CodeGen.Program as CGP
import           Obsidian.GCDObsidian.CodeGen.InOut

import Obsidian.GCDObsidian.Program
import Obsidian.GCDObsidian.Exp
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Blocks
import Obsidian.GCDObsidian.Array
import Obsidian.GCDObsidian.Library
import Obsidian.GCDObsidian.Force


import Data.Word
import Data.Int
import Data.Bits

import qualified Data.Vector.Storable as V

import Control.Monad.State

import Prelude hiding (zipWith,sum)

---------------------------------------------------------------------------
-- MapFusion example
---------------------------------------------------------------------------

mapFusion :: Array Pull EInt -> Program (Array Pull EInt)
mapFusion arr =
  do
    a1 <- sync $ (fmap (+1) . fmap (*2)) arr
    sync $ (fmap (+1) . fmap (*2)) a1

input1 :: Array Pull EInt 
input1 = namedArray "apa" 32

-- Uses genKernel, that implicitly maps over all blocks. 
-- getMapFusion   = putStrLn$ CUDA.genKernel "mapFusion" mapFusion input1
-- getMapFusion_  = putStrLn$ CL.genKernel_ "mapFusion" mapFusion input1

---------------------------------------------------------------------------
-- Sync, Force. What to use? what to scrap ? 
---------------------------------------------------------------------------
-- very limiting type.. 
sync :: Scalar a => Array Pull (Exp a) -> Program (Array Pull (Exp a))
sync = force

             
---------------------------------------------------------------------------
-- Global array permutation
---------------------------------------------------------------------------
rev :: Array Pull a -> Array Pull a
rev (Array n (Pull ixf)) =
  Array n (Pull (\ix -> ixf ((fromIntegral n) - 1 - ix)))

reverseG :: Blocks (Array Pull a) -> Blocks (Array Pull a)
reverseG (Blocks nb arrf) =
  Blocks nb (\bix -> rev (arrf (nb - 1 - bix)))


-- Permutations on the output arrays are more complicated
-- good wrappings are needed!
reverseGO :: Blocks (Array Push a)
             -> Blocks (Array Push a)
reverseGO (Blocks nb prgf) =
  Blocks nb $ 
   \bix ->
    let a@(Array n (Push p)) =  prgf bix
    in  Array n $
        Push  (\k ->
               let k' k (ix,e) = k ((fromIntegral n) - 1 - ix,e)
               in  p (k' k))
               -- k :: (Exp Word32,EInt) -> Program

---------------------------------------------------------------------------
-- Global Array examples 
---------------------------------------------------------------------------

mapSomething :: Array Pull EInt -> Array Push EInt
mapSomething arr = push ((fmap (+1) . fmap (*2)) arr)



inputG :: Blocks (Array Pull EInt) 
inputG = namedGlobal "apa" (variable "N") 256



testG1 :: Blocks (Array Pull EInt) -> Program (Blocks (Array Pull EInt))
testG1 arr = force ( fmap mapSomething (reverseG arr) )

getTestG1 = putStrLn$ CUDA.genKernel "testG1" testG1 inputG

testG2 :: Blocks (Array Pull EInt)
          -> Blocks (Array Pull EInt)
          -> Program (Blocks (Array Pull EInt))
testG2 _ arr = force ( fmap mapSomething (reverseG arr) )


---------------------------------------------------------------------------
-- Print Programs for test
---------------------------------------------------------------------------
prg0 = putStrLn$ printPrg$ mapFusion input1
prg1 = putStrLn$ printPrg$ testG1 inputG


---------------------------------------------------------------------------
-- Translate and pring as CGP.Programs 
---------------------------------------------------------------------------
prg0' = putStrLn$ CGP.printPrg$ CGP.runPrg (mapFusion input1)
prg1' = putStrLn$ CGP.printPrg$ CGP.runPrg (testG1 inputG)

---------------------------------------------------------------------------
-- A small test for the function "reifyer" 
---------------------------------------------------------------------------
reify0 = fst $ toProgram 0 testG2 (inputG :-> inputG)






---------------------------------------------------------------------------
-- Counting sort experiments
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Histogram
---------------------------------------------------------------------------
histogram :: Exp Word32
             -> Blocks (Array Pull (Exp Word32))
             -> Blocks (Array Push (Exp Word32))
histogram maxLen (Blocks nb blkf)  =
  Blocks nb blkf' 
  where
                
    blkf' bid = Array blkSize (Push (collect bid)) 
    blkSize = len (blkf 0) -- all blocks are same size  
    collect bid k = ForAll blkSize $ \i ->
      k ((blkf bid) ! i,1)

hist
  :: Exp Word32
     -> Blocks (Array Pull (Exp Word32))
     -> Program (Blocks (Array Pull (Exp Word32)))
hist max inp = force  (histogram max inp)

inputWord32 :: Blocks (Array Pull (Exp Word32)) 
inputWord32 = namedGlobal "apa" (variable "N") 256


getHist = putStrLn$ CUDA.genKernel "hist" (hist 256)  inputWord32


---------------------------------------------------------------------------
-- Scan 
---------------------------------------------------------------------------
sklanskyLocal
  :: Scalar a =>
     Int
     -> (Exp a -> Exp a -> Exp a)
     -> Array Pull (Exp a)
     -> Program (Array Pull (Exp a))
sklanskyLocal 0 op arr = return (id arr)
sklanskyLocal n op arr =
  do 
    let arr1 = twoK (n-1) (fan op) arr
    arr2 <- sync arr1
    sklanskyLocal (n-1) op arr2
                     

fan op arr = conc (a1, fmap (op c) a2) 
    where 
      (a1,a2) = halve arr
      c = a1 ! (fromIntegral (len a1 - 1))


sklanskyAllBlocks :: Int
                     -> Blocks (Array Pull (Exp Int32))
                     -> Program (Blocks (Array Pull (Exp Int32)))
sklanskyAllBlocks logbsize arr =
  force $ fmap (sklanskyLocal logbsize (+)) arr
   


getScan n = CUDA.genKernel "scan" (sklanskyAllBlocks n) 
                    (namedGlobal "apa" (variable "N") (2^n)
                     :: Blocks (Array Pull (Exp Int32)))


getScan_ n = CUDA.genKernel_ "scan" (sklanskyAllBlocks n) 
                    (namedGlobal "apa" (variable "N") (2^n)
                     :: Blocks (Array Pull (Exp Int32)))

-- TODO: Rewrite Scan with BlockMap functionality.
--       Also add the output of blockmaxs, and tweak code generation to
--       allow such kernels. 
---------------------------------------------------------------------------
-- Reconstruct
---------------------------------------------------------------------------

type GlobalArray p a = Blocks (Array p a) 

reconstruct :: Exp Word32
               -> GlobalArray Pull (Exp Word32)
               -> GlobalArray Pull (Exp Word32)
               -> Program (GlobalArray Pull (Exp Word32))
reconstruct nb inp pos = force $ 
  Blocks nb (\bix -> -- nb seems totally unimportant
                     -- (does appear in code). rethink this.
    Array bsize
    $ Push (\k -> do
        ForAll bsize
          (\ix -> k ((pos !| bix) ! ix , 
                     (inp !| bix) ! ix))))
  where
    bsize = len (inp !| 0) 
-- Check that reconstruct does what it is suppoed to
-- TODO: Needs type convertion functionality if this is
--       to be implemented for anything else than Word32.
--         (indices are Word32)
getReconstruct n =
  CUDA.genKernel "reconstruct"
                 (reconstruct (variable "N"))
                 (inG n :-> inG n)
                 
inG n = namedGlobal "apa" (variable "N") (2^n)
        :: Blocks (Array Pull (Exp Word32))

---------------------------------------------------------------------------
-- Testing WithCUDA aspects
---------------------------------------------------------------------------

testGRev :: Blocks (Array Pull EWord32)
            -> Program (Blocks (Array Pull EWord32))
testGRev arr = force ( fmap (push . rev) arr )


wc1 = 
  withCUDA $
  do
    -- Capture, compile and link the Obsidian program
    -- into a CUDA function 
    myCudaFun <- capture testGRev inputWord32
    
    -- Set up data and launch the kernel!
    useVector (V.fromList [0..511::Word32]) $ \ inp -> 
      allocaVector 512 $ \out ->
        do
          execute myCudaFun
                  2  -- how many blocks 
                  0   -- how much shared mem (will come from an analysis later) 
                  inp out 
          r <- lift$ CUDA.peekListArray 512 out
          lift $ putStrLn $ show  (r :: [Word32])

 
t2 =
  do
    let str = getScan 8
    fp <- storeAndCompile "-arch=sm_30" "scan.cu" (header ++ str)
    putStrLn fp
    where
      header = "#include <stdint.h>\n"


cs =
  withCUDA $
  do
    skl <- capture (sklanskyAllBlocks 8) (sizedGlobal (variable "N") 256)

    lift $putStrLn $ show $ kThreadsPerBlock skl
    
    useVector (V.fromList (replicate 256 (1::Word32))) $ \ inp -> 
      useVector (V.fromList (replicate 256 (0::Word32))) $ \out ->
        do
          execute skl
                  1  -- how many blocks 
                  (2*256*4)  
                  inp out 
          r <- lift$ CUDA.peekListArray 256 out
          lift $ putStrLn $ show  (r :: [Word32])
