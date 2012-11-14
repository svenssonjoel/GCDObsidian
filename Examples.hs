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
sync = force . push

---------------------------------------------------------------------------
-- map over blocks. Normal Functor instance is fine! 
---------------------------------------------------------------------------
instance Functor Blocks where
  fmap f (Blocks nb bxf) = Blocks nb (\bix -> (f (bxf bix)))

---------------------------------------------------------------------------
-- zipWith. (Normal zipWith)
---------------------------------------------------------------------------
zipBlocksWith :: (a -> b -> c)
                 -> Blocks a -> Blocks b -> Blocks c
zipBlocksWith f (Blocks nb1 bxf1)
                (Blocks nb2 bxf2) =
  Blocks (min nb1 nb2) (\bix -> f (bxf1 bix) (bxf2 bix))
   
---------------------------------------------------------------------------
-- forceBlocks
---------------------------------------------------------------------------
-- forceBlocks :: Blocks (Program a) -> Program (Blocks a)
-- cannot be this general.. 

-- Trying a very limited form, will need type classes... 
-- Sometimes a forall is needed. I don't really see a pattern in when. 
forceBlocks :: forall a. Scalar a => Blocks (Array Push (Exp a))
               -> Program (Blocks (Array Pull (Exp a)))
forceBlocks (Blocks n bxf) =  
  do
    global <- Output $ Pointer (typeOf (undefined :: (Exp a))) 

    -- dryrun to get length. 
    let (Array s (Push pfun)) =  bxf (variable "dummy") 
    
    ForAllBlocks n
      (\bid ->
        do
          let (Array s (Push pfun)) = bxf bid 
          pfun (assignTo global (bid, s)))
     
    return $ Blocks n  $ 
             \bix -> Array s (Pull (\ix -> index global ((bix * (fromIntegral s)) + ix)))
      where 
        assignTo name (bid,s) (i,e) = Assign name ((bid*(fromIntegral s))+i) e 


forceBlocks' :: forall a. Scalar a => Blocks (Program (Array Pull (Exp a)))
               -> Program (Blocks (Array Pull (Exp a)))
forceBlocks' (Blocks n bxf) =  
  do
    global <- Output $ Pointer (typeOf (undefined :: (Exp a))) 

    -- dryrun to get length. 
    (Array s (Pull pfun)) <- bxf (variable "dummy") 
    
    ForAllBlocks n
      (\bid ->
        do
          arr <- bxf bid
          let (Array s (Push pfun)) = push arr 
          pfun (assignTo global (bid, s)))
     
    return $ Blocks n  $ 
             \bix -> Array s (Pull (\ix -> index global ((bix * (fromIntegral s)) + ix)))
      where 
        assignTo name (bid,s) (i,e) = Assign name ((bid*(fromIntegral s))+i) e 

          
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
testG1 arr = forceBlocks ( fmap mapSomething (reverseG arr) )

getTestG1 = putStrLn$ CUDA.genKernel "testG1" testG1 inputG

testG2 :: Blocks (Array Pull EInt)
          -> Blocks (Array Pull EInt)
          -> Program (Blocks (Array Pull EInt))
testG2 _ arr = forceBlocks ( fmap mapSomething (reverseG arr) )


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
hist max inp = forceBlocks  (histogram max inp)

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


sklanskyAllBlocks :: Int -> Blocks (Array Pull (Exp Int32)) -> Program (Blocks (Array Pull (Exp Int32)))
sklanskyAllBlocks logbsize arr = forceBlocks' $ fmap (sklanskyLocal logbsize (+)) arr
   


getScan n = CUDA.genKernel "scan" (sklanskyAllBlocks n) 
                    (namedGlobal "apa" (variable "N") (2^n) :: Blocks (Array Pull (Exp Int32)))


getScan_ n = CUDA.genKernel_ "scan" (sklanskyAllBlocks n) 
                    (namedGlobal "apa" (variable "N") (2^n) :: Blocks (Array Pull (Exp Int32)))

-- TODO: Rewrite Scan with BlockMap functionality.
--       Also add the output of blockmaxs, and tweak code generation to
--       allow such kernels. 
---------------------------------------------------------------------------
-- Distribute
---------------------------------------------------------------------------


---------------------------------------------------------------------------
-- Testing WithCUDA aspects
---------------------------------------------------------------------------

testGRev :: Blocks (Array Pull EWord32)
            -> Program (Blocks (Array Pull EWord32))
testGRev arr = forceBlocks ( fmap (push . rev) arr )


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
          lift$ CUDA.launchKernel myCudaFun
                                  (2,1,1)   -- number of Blocks
                                  (256,1,1) -- Threads per block
                                  0         -- amount of shared mem
                                  Nothing
                                  [CUDA.VArg inp, CUDA.VArg out]
          r <- lift$ CUDA.peekListArray 512 out
          lift $ putStrLn $ show  (r :: [Word32])

 
t2 =
  do
    let str = getScan 8
    fp <- storeAndCompile "-arch=sm_30" "scan.cu" (header ++ str)
    putStrLn fp
    where
      header = "#include <stdint.h>\n"
 